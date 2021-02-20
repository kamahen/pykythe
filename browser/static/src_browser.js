'use strict';

// Implementation of annotated source browser, using nodes defined by
// src_browser.html.
// For the overall structure of this, see
// https://github.com/kamahen/swipl-server-js-client/blob/master/static/simple_client.js

// http://localhost:9999?corpus=CORPUS&root=ROOT&path=tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/a11.py&#L11
// http://localhost:9999?corpus=CORPUS&root=ROOT&path=home/peter/src/pykythe/pykythe/ast_raw.py#L81
//    which does redirect to:
// http://localhost:999/static/src_browser.html?corpus=...
// and we depend on the server to handle redirections transparently,
// per the HTTP specs.

// TODO: The code is inconsistent in whether it has corpus, root, path as
//       separate items or combined into 'corpus/root/path'.
//       Scan for newFromCombined.

// SourceItem class encapsulates corpus/root/path/lineno/hilite into a
// single object (lineno is optional and defaults to 1; hilite is
// semantic object and defaults to null), with some convenience
// methods and constructors.
class SourceItem {
    constructor(corpus, root, path, lineno, hilite) {
        // TODO: escape '/' in corpus, root
        this.corpus = corpus || '';
        this.root = root || '';
        this.path = path || '';
        this.lineno = lineno || 1;
        this.hilite = hilite || null;
        console.assert(this.corpus.indexOf('/') < 0, 'Invalid corpus (contains "/")', corpus);
        console.assert(this.root.indexOf('/') < 0, 'Invalid root (contains "/")', root);
    }

    static newFromCombined(corpus_root_path, lineno, hilite) {
        // TODO: escape '/' in corpus, root
        // Note that src_browser.pl currently verifies that corpus and root don't have '/' in them.
        const corpus_root_path_split = corpus_root_path.split('/');
        return new SourceItem(corpus_root_path_split[0], // corpus
                              corpus_root_path_split[1], // root
                              corpus_root_path_split.slice(2).join('/'), // path
                              lineno, hilite);
    }

    static newFromJSON(json) {
        return new SourceItem(json.corpus, json.root, json.path, json.lineno);
    }

    pathItems() {
        // ''.split('') == [''] but we want []
        return [this.corpus, this.root]
            .concat((this.path === '') ? [] : this.path.split('/'));
    }

    combinedFilePath() {
        // TODO: escape '/' in corpus, root
        return this.corpus + '/' + this.root + '/' + this.path;
    }

    toString() {
        return combinedFilePath + '#' + linenoId(this.lineno);
    }
}

// In the following the Corpus, Root, Language of the semantic are
// encoded as "Corpus</>Root</>Language</>fullyQualifedSemantic".

// Global mapping anchor signature to anchor signatures that is used to
// highlight when the mouse moves over an anchor. An anchor always maps
// to itself, plus possibly some more.
var g_anchor_to_anchors = {};

// Global mapping anchor signature to semantic signatures.
var g_anchor_to_semantics = {};

// Global mapping semantic signature to anchor signatures.
var g_semantic_to_anchors = {};

// global tree of dir/file entries - set by dynamic load from server
// TODO: can we get rid of this (singleton) global?
var g_file_tree = null;

// Map the ast_color.Color.token_color enumeration to CSS class names
const token_css_color_class = {  // See src_browser.css
    '<ARG_KEYWORD>':        'python_arg_keyword',
    '<ATTR_BINDING>':       'python_attr_binding',
    '<ATTR_REF>':           'python_attr_ref',
    '<BARE>':               'python_bare',
    '<COMMENT>':            'python_comment',
    '<KEYWORD>':            'python_keyword',
    '<NEWLINE>':            'python_newline', // for completeness
    '<NUMBER>':             'python_number',
    '<PUNCTUATION>':        'python_punctuation',
    '<PUNCTUATION_REF>':    'python_punctuation_ref',
    '<STRING>':             'python_string',
    '<VAR_BINDING>':        'python_var_binding',
    '<VAR_BINDING_GLOBAL>': 'python_binding_global',
    '<VAR_REF>':            'python_var_ref',
    '<WHITESPACE>':         'python_whitespace', // for completeness
};

// Map the ast_color.Color.token_color enumeration to whether it's a
// a "token" or not - a "token" can have a link (hover) on it.
const is_token_name = {
    '<ARG_KEYWORD>':        false, // TODO: multiple keyword colors?
    '<ATTR_BINDING>':       true,
    '<ATTR_REF>':           true,
    '<BARE>':               true,
    '<COMMENT>':            false,
    '<KEYWORD>':            false,
    '<NEWLINE>':            false,
    '<NUMBER>':             false,
    '<PUNCTUATION>':        false,
    '<PUNCTUATION_REF>':    true,
    '<STRING>':             false,
    '<VAR_BINDING>':        true,
    '<VAR_BINDING_GLOBAL>': true,
    '<VAR_REF>':            true,
    '<WHITESPACE>':         false,
};

// Map a path item type ('dir' or 'file') to a class in the dropdown
const path_type_to_class = {
    'dir':  'file_nav_sel_dir',
    'file': 'file_nav_sel_file',
};

// Callback from <body onload="renderPage();">
async function renderPage() {
    // https://developers.google.com/web/updates/2016/01/urlsearchparams
    const params = new URLSearchParams(location.search);
    const lineno = location.hash ? idLineno(location.hash) : 1;
    await fetchFromServer(
        {src_file_tree: ''},
        file_tree_from_server => setFileTree(
            file_tree_from_server,
            new SourceItem(
                params.get('corpus'),
                params.get('root'),
                params.get('path'),
                lineno,
                params.get('hilite'))));
}

// Callback from server fetch of the file navigation tree
function setFileTree(file_tree_from_server, source_item) {
    g_file_tree = file_tree_from_server;
    displayFileTree(source_item);
}

// Display file tree (id='file_nav')
async function displayFileTree(source_item) {
    const path_items = source_item.pathItems();
    var tree = fileNavElement();
    while (tree.firstChild) {
        tree.firstChild.remove();
    }
    await displayFileTreeItems(0, path_items, g_file_tree, source_item.lineno, source_item.hilite);
}

// Recursive function for displaying the remaining file tree
// navigation items, starting from item_i (0-indexed).
// The path_items are used to "pre-select" items in the drop-downs.
async function displayFileTreeItems(item_i, path_items, file_tree_nodes, lineno, hilite) {
    // file_tree_nodes is a list of 'file' or 'dir' items
    // TODO: for now, corpus and root are treated as path items

    var dropdown = null;
    if (path_items.length == 0) {
        if (file_tree_nodes.length > 1) {
            dropdown = createDropdown(item_i, 'dir');
            // The following is multiple Unicode em-dashes:
            addDropdownOption(dropdown, '————', 'dir',
                              'nav_sel-' + file_tree_nodes.path);
        } else {
            dropdown = createDropdown(item_i, 'dir');
        }
        for (const tree_item of file_tree_nodes) {
            addDropdownOption(dropdown, tree_item.name, tree_item.type,
                              'nav_sel-' + tree_item.path);
        }
        dropdown.selectedIndex = 0;
    } else {
        dropdown = createDropdown(item_i, 'dir');
        for (const [i, tree_item] of file_tree_nodes.entries()) {
            // TODO: make directories display differently
            addDropdownOption(dropdown, tree_item.name, tree_item.type,
                              'nav_sel-' + tree_item.path);
            if (tree_item.name == path_items[0]) {
                dropdown.selectedIndex = i;
            }
        }
    }
    fileNavElement().appendChild(dropdown);
    const selected_node = file_tree_nodes[dropdown.selectedIndex];

    if (path_items.length > 0) {
        if (selected_node.type == 'dir') {
            await displayFileTreeItems(item_i + 1, path_items.slice(1),
                                       selected_node.children, lineno, hilite);
        } else if (selected_node.type == 'file') {
            await displayNewSrcFile(SourceItem.newFromCombined(selected_node.path, lineno, hilite));
        } else {
            return alert('Bad file_tree_nodes.type: ' + selected_node[0].type);
        }
    } else if (file_tree_nodes.length == 1) {
        if (file_tree_nodes[0].type == 'dir') {
            await displayFileTreeItems(item_i + 1, [], file_tree_nodes[0].children, lineno, hilite);
        } else if (file_tree_nodes[0].type == 'file') {
            await displayNewSrcFile(SourceItem.newFromCombined(selected_node.path, lineno, hilite));
        } else {
            return alert('Bad file_tree_nodes.type: ' + file_tree_nodes[0].type);
        }
    }
}

// Create a dropdown (<SELECT ...>) element
function createDropdown(i, type) {
    var dropdown = document.createElement('select');
    dropdown.setAttribute('class', path_type_to_class[type]);
    // dropdown.id = 'path-' + i;
    dropdown.onclick = async function(e) {
        const  t = e.currentTarget;
        const  t_selected = t[t.selectedIndex];
        if (t_selected.id.startsWith('nav_sel-')) {
            // TODO: location.href = location.origin + '?' +
            //         corpus=CORPUS&root=ROOT&path=home/peter/src/pykythe/pykythe/ast_raw.py#L81
            // and omit displayFileTree ... but this requires a bit of work
            // to avoid an infinite loop
            displayFileTree(SourceItem.newFromCombined(t_selected.id.substr('nav_sel-'.length)));
        }
        e.preventDefault();
    };
    return dropdown;
}

// Add an <OPTION ...> element to a dropdown
function addDropdownOption(dropdown, text, type, id) {
    var option = document.createElement('option');
    option.setAttribute('class', path_type_to_class[type]);
    option.text = text;
    option.id = id;
    dropdown.add(option);
}

// Callback from file tree navigation click, to load a file into the
// fileNavElement() via displaySrcContents.
async function displayNewSrcFile(source_item) {
    // Update the displayed URL, to allow navigating back to this page.
    // See setXrefEdgeLinkItem() for how the URL is created.
    // TODO: is there a better choice for the 1st arg to replaceState?
    history.replaceState(
        {}, 'Pykythe Browser',
        location.origin + location.pathname +
            '?corpus=' + encodeURIComponent(source_item.corpus) +
            '&root=' + encodeURIComponent(source_item.root) +
            '&path=' + encodeURIComponent(source_item.path) +
            (source_item.hilite ? '&hilite=' + encodeURIComponent(source_item.hilite) : '') +
            '#L' + encodeURIComponent(source_item.lineno));
    var progress = document.createElement('span');
    progress.innerHTML = '&nbsp;&nbsp;&nbsp;Fetching file ' +
        sanitizeText(source_item.combinedFilePath()) + ' ...';
    fileNavElement().appendChild(progress);
    // TODO: alert if fetch fails
    await fetchFromServer(
        {src_browser_file: {corpus: source_item.corpus,
                            root: source_item.root,
                            path: source_item.path}},
        color_data => displaySrcContents(source_item, color_data));
}

// Callback from server fetch of a single source file (from displayNewSrcFile)
// source_item: SourceItem
// color_data is a map with corpus, language, path, root, lines;
// lines is an array of arrays: each item having
//    lineno,column,start,end,signature,token_color,value,edges
//  each in edges having edge,target{corpus,language,path,root,signature}
function displaySrcContents(source_item, color_data) {
    g_anchor_to_anchors = color_data.anchor_to_anchors;
    g_anchor_to_semantics = color_data.anchor_to_semantics;
    g_semantic_to_anchors = color_data.semantic_to_anchors;
    console.assert(source_item.corpus === color_data.corpus &&
                   source_item.root === color_data.root &&
                   source_item.path === color_data.path,
                   'Mismatch source-item, color_data',
                   'source', source_item,
                   'color', color_data);
    fileNavElement().lastChild.innerHTML =
        'Rendering file ' + source_item.combinedFilePath() + '...';
    var table = document.createElement('table');
    table.setAttribute('class', 'src_table');
    for (const [line_key, line_parts] of color_data.lines.entries()) {
        var row = table.insertRow();
        var td1 = row.insertCell();
        td1.setAttribute('class', 'src_lineno');
        td1.id = linenoId(line_key + 1);  // TODO: Fix the 0-origin line_key (the source display is 1-origin)
        var td2 = row.insertCell();
        td2.setAttribute('class', 'src_line');
        var txt_span = document.createElement('span');
        if (line_parts.length) {
            td1.appendChild(document.createTextNode(line_parts[0].lineno));
            srcLineTextHoverable(line_parts, txt_span, source_item);
        } else {
            txt_span.innerHTML = '&nbsp;';
        }
        td2.appendChild(txt_span);
    }
    replaceChildWith('src', table);
    // console.log('DISPLAY_SRC_CONTENTS', source_item);  // DO NOT SUBMIT
    scrollIntoViewAndMark(source_item.lineno, source_item.hilite, source_item);
    fileNavElement().lastChild.remove(); // Remove status message
}

// Simplified source display (no active links)
// See also srcLineTextHoverable()
function srcLineTextSimple(line_parts, txt_span, data_semantics) {
    for (const line_part of line_parts) {
        var span = document.createElement('span');
        span.setAttribute('class', token_css_color_class[line_part.token_color]);
        // TODO: the following doesn't hilite - why?  DO NOT SUBMIT
        if (is_token_name[line_part.token_color] &&
            (data_semantics || []).indexOf(g_anchor_to_semantics[line_part.signature]) >= 0) {
            span.classList.add('src_hilite');
        }
        span.innerHTML = sanitizeText(line_part.value);
        txt_span.appendChild(span)
    }
}

// Display a single line of a source display
function srcLineTextHoverable(line_parts, txt_span, source_item) {
    for (const line_part of line_parts) {
        var span = document.createElement('span');
        span.setAttribute('class', token_css_color_class[line_part.token_color]);
        span.innerHTML = sanitizeText(line_part.value);
        if (is_token_name[line_part.token_color] && line_part.signature) {
            // TODO: remove this code (when we know we won't need g_anchor_edges):
            // for (const p_edge of line_part.edges) {
            //     g_anchor_edges.push({anchor_signature: line_part.signature,
            //                          edge: p_edge.edge,
            //                          target: p_edge.target});
            // }
            span.id = line_part.signature;  // DO NOT SUBMIT -- should have a known prefix on the id
            span.onmouseover = async function(e) { // e is MouseEvent
                mouseoverAnchor(e.currentTarget, 'add', 'src_hover', source_item);
                e.preventDefault();
            };
            span.onmouseleave = async function(e) { // e is MouseEvent
                mouseoverAnchor(e.currentTarget, 'remove', 'src_hover', source_item);
                e.preventDefault();
            };
            span.onclick = async function(e) { // e is MouseEvent
                await clickAnchor(e.currentTarget, source_item);
                e.preventDefault();
            };
            // TODO: handle right-click
            // span.oncontextmenu = async function(e) { // e is MouseEvent
            //     e.preventDefault();
            // };
        } else if (is_token_name[line_part.token_color]) {
            console.assert(line_part.signature,
                           'SIGNATURE SHOULD NOT BE EMPTY',
                           line_part);
        } else {
            console.assert(! line_part.signature,
                           'SIGNATURE SHOULD BE EMPTY',
                           line_part);
        }
        txt_span.appendChild(span);
    }
}

// Do an action ('add' or 'remove') for an item in a class list
// - callback from mouseover/leave on a token (anchor) in the source display
// This loops over all the elements connected to the target
// (g_anchor_to_anchors) and applies the action (add/remove) of the
// class_id. Typically, the class_id is 'src_hover', which highlights
// the item.
function mouseoverAnchor(mouse_target, class_action, class_id, source_item) {
    for (const anchors2 of (g_anchor_to_anchors[mouse_target.id] || [])) {
        const sig2 = document.getElementById(anchors2);
        if (sig2) {
            // sig2.classList.{add,remove}(class_id):
            sig2.classList[class_action](class_id);
        } else {
            if (class_action === 'add') {
                console.log('No edge for ' + mouse_target.id + ' ' + signature2);
                // alert('No edge for ' + mouse_target.id + ' ' + signature2);  // DO NOT SUBMIT
            }
        }
    }
}

// Callback for a click on a token (anchor) in the source display
async function clickAnchor(mouse_target, source_item) {
    // console.log('CLICK ' + mouse_target.id + ' in ' + source_item.combinedFilePath());
    await fetchFromServer(
        {anchor_xref: {signature: mouse_target.id,
                       corpus: source_item.corpus,
                       root: source_item.root,
                       path: source_item.path,
                       language: 'python'}},  // DO NOT SUBMIT - don't hard-code language
        data => setXref(source_item, mouse_target.id, data));
}

// Callback from getting Kythe facts for a token (anchor) click
function setXref(source_item, signature, data) {
    // expected out-edges for anchor: defines, defines/binding, ref, ref/call
    document.getElementById('xref').innerHTML = 'Getting Kythe links for ' + source_item.combinedFilePath() + ' anchor:' + signature + ' ...';

    var table = document.createElement('table');
    setXrefItemHeader(table, signature, data);
    setXrefNodeValues(table, data);
    setXrefEdgeLinks(table, data, signature, source_item);
    setXrefBottom(table);
}

// In the xref area, display the item signature as a header
function setXrefItemHeader(table, signature, data) {
    table.setAttribute('class', 'src_table');  // DO NOT SUBMIT - should we have a new CSS class for this?
    var row_cell = tableInsertRowCell(table);
    // TODO: remove <i> using row_cell.settAttribute('class', some-other-class)
    if (data.semantics.length == 0) {
        row_cell.appendChild(document.createElement('span')).innerHTML + '<i>(no semantics)</i>';
    } else {
        row_cell.appendChild(document.createElement('span')).innerHTML = data.semantics.map(
            s => '<i>' + sanitizeText(s.signature) + '</i>').join('<br/>');
    }
}

// In the xref area, display the node values
function setXrefNodeValues(table, data) {
    for (const nv of data.semantic_node_values) {
        // TODO: use class attributes:
        tableInsertRowCellHTML(table, '&nbsp;&nbsp;<b>' +
                               sanitizeText(nv.kind) +
                               '</b>:&nbsp;' +
                               sanitizeText(nv.value));
    }
}

function setXrefEdgeLinks(table, data, signature, source_item) {
    for (const edge_links of data.edge_links) {
        setXrefEdgeLinkHead(table, edge_links);
        for (const path_link of edge_links.links) {
            setXrefEdgeLinkItem(table, path_link, data, signature, source_item);
        }
    }
}

function setXrefEdgeLinkHead(table, edge_links) {
    var row_cell = tableInsertRowCell(table);
    row_cell.setAttribute('class', 'xref_head');
    cellHTML(
        row_cell,
        sanitizeText(
            edge_links.edge + ' (' +
                singularPlural(edge_links.links.length, 'file', 'files') +
                ', ' +
                singularPlural(
                    sumList(edge_links.links.map(link => link.lines.length)),
                    'line', 'lines') +
                ')'));
}

function setXrefEdgeLinkItem(table, path_link, data, signature, source_item) {
    {
        var row_cell = tableInsertRowCell(table);
        // DO NOT SUBMIT - use a CSS class for '<b><i>':
        cellHTML(row_cell,
                 '<b><i>' + sanitizeText(path_link.path) + '</i></b>');
    }
    for (const link_line of path_link.lines) {
        var row_cell = tableInsertRowCell(table);
        const lineno_span = row_cell.appendChild(document.createElement('a'));
        lineno_span.title = sanitizeText('xref-title-lineno'); // DO NOT SUBMIT - addd semantic signature to xref
        const semantic = g_anchor_to_semantics[signature];
        const href = location.origin + location.pathname +
              '?corpus=' + encodeURIComponent(link_line.corpus) +
              '&root=' + encodeURIComponent(link_line.root) +
              '&path=' + encodeURIComponent(link_line.path) +
              (semantic ? '&hilite=' + encodeURIComponent(semantic) : '') +
              '#' + encodeURIComponent(linenoId(link_line.lineno));
        lineno_span.href = href;
        lineno_span.innerHTML = '<b><i>' + link_line.lineno + ':&nbsp;</i></b>';  // DO NOT SUBMIT - CSS class, rowspan
        var txt_span = row_cell.appendChild(document.createElement('a'));
        txt_span.title = sanitizeText('xref-title-lineno'); // DO NOT SUBMIT - addd semantic signature
        txt_span.href = href;
        // For the case of same file, the href-fetch doesn't cause the
        // lineno-handling stuff to be executed, so do it ourselves:
        if (link_line.corpus == source_item.corpus &&
            link_line.root == source_item.root &&
            link_line.path == source_item.path) {
            txt_span.onclick = async function (e) {
                scrollIntoViewAndMark(link_line.lineno, semantic, link_line);
                e.preventDefault();  // Prevent standard behavior of scrollIntoView({block: 'start'})
            }
        }
        srcLineTextSimple(link_line.line, txt_span, data.semantics);
    }
}

function setXrefBottom(table) {
    var row_cell = tableInsertRowCell(table);
    cellHTML(row_cell, '&nbsp;');  // ensure some space at the bottom
    replaceChildWith('xref', table);
}

function tableInsertRowCell(table) {
    return table.insertRow().insertCell();
}

function cellHTML(cell, html) {
    cell.appendChild(document.createElement('span')).innerHTML = html
}

function tableInsertRowCellHTML(table, html) {
    cellHTML(tableInsertRowCell(table), html);
}

function singularPlural(number, singular, plural) {
    if (number == 1) {
        return 'one ' + singular;
    } else {
        return number + ' ' + plural;
    }
}
// There shouldn't be a need for .replace(/ /g, '&nbsp;') if CSS
// has white-space:pre ... but by experiment, it's needed.
const sanitize_re = /[&<>\n "'`/]/ig;
const sanitizeMap = {
    '&': '&amp;',
    '<': '&lt;',
    '>': '&gt;',
    '\n': '<br/>', // TODO: not needed? - put into extract_color.pl
    ' ': '&nbsp;', // TODO: add test for tabs, line-feed, etc. in source
    '"': '&quot;',
    "'": '&apos;',
    '`': '&grave;',
    '/': '&#x2F;',
};

// Sanitize a string, allowing HTML tags and special characters to not cause problems
// TODO: is there a builtin function for this? (cf encodeURIComponent)
function sanitizeText(raw_str) {
    // DO NOT SUBMIT - FIXME: figure out where raw_str can be null and fix
    return raw_str
        ? raw_str.replace(sanitize_re, (match)=>(sanitizeMap[match]))
        : '';
}

// Send a request to the server and schedule a callback.
async function fetchFromServer(request, callback) {
    // callback should take a single arg, the response from the server,
    // e.g.: json_data => set_corpus_root_path_filename(json_data))
    try {
        const response = await fetch(
            '/json',
            {method: 'POST',
             headers: {'Content-Type': 'application/json'},
             body: JSON.stringify(request),
             mode: 'cors',                  // Don't need?
             cache: 'no-cache',             // Don't need?
             credentials: 'same-origin',    // Don't need?
             redirect: 'follow',            // Don't need?
             referrerPolicy: 'no-referrer', // Don't need?
            });
        callback(await response.json());
    } catch(err) {
        alert('fetch ' + JSON.stringify(request) + ': ' + err);
    }
}

function jq(id) {
    // TODO: unused?
    return '.' + id.replace( /(:|\.|\[|\]|,|=|@|<|>)/g, '\\$1' );
}

// Sum the items in a list (array)
function sumList(items) {
    return items.reduce((accum, value) => accum + value, 0);
}

// Clear an element and replace with a single child
function replaceChildWith(id, new_child) {
    var elem = document.getElementById(id);
    // elem.replaceChild(new_child, elem.firstChild);
    deleteAllChildren(elem);
    elem.appendChild(new_child);
}

// Clear an element
function deleteAllChildren(elem) {
    while (elem.firstChild) {
        elem.firstChild.remove();
    }
}

// Convenience function: get the 'file_nav' element
function fileNavElement() {
    return document.getElementById('file_nav');
}

// Convert a lineno to an ID
function linenoId(lineno) {
    return 'L' + lineno;
}

function scrollIntoViewAndMark(lineno, hilite, debug_item) {
    if (lineno) {
        const line_elem = document.getElementById(linenoId(lineno));
        if (line_elem) {
            // TODO: remove any other 'src_lineno_hilite' attributes
            // TODO: ensure behavior is same as hash ('#' + linenoId(lineno))
            // Choices for block/inline are: 'start', 'center', 'end', 'nearest'
            line_elem.scrollIntoView({block: 'center', inline: 'start'});
            // TODO: highlight the source text as well as the line #
            line_elem.setAttribute('class', 'src_lineno_hilite');
        } else {
            console.log('NO LINE:', linenoId(lineno), debug_item);
        }
    } else {
        console.log('NO LINENO', debug_item);
    }
    // see mousoverAnchor() - combine some of the source
    for (const anchors2 of g_semantic_to_anchors[hilite] || []) {
        const sig2 = document.getElementById(anchors2);
        if (sig2) {
            sig2.classList.add('src_hover');
        } else {
            console.log('No edge for ' + mouse_target.id + ' ' + signature2);
            // alert('No edge for ' + mouse_target.id + ' ' + signature2);  // DO NOT SUBMIT
        }
    }
}

// Convert an ID or #ID (from location.hash) to a lineno
function idLineno(id) {
    var lineno;
    if (id.substr(0, 1) == 'L') {
        lineno = parseInt(id.substr(1));
    } else if (id.substr(0, 2) == '#L') {  // From location.hash
        lineno = parseInt(id.substr(2));
    } else {
        lineno = NaN;
    }
    if (isNaN(lineno)) {
        console.log('Invalid lineno ID: "' + id + '"');
        lineno = 1;
    }
    return lineno;
}

// End of file
