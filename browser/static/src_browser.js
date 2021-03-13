'use strict';

// Implementation of annotated source browser, using nodes defined by
// src_browser.html.
// For the overall structure of this client code, see
// https://github.com/kamahen/swipl-server-js-client/blob/master/static/simple_client.js

// A URL such as
//   http://localhost:9999?corpus=CORPUS&root=ROOT&path=home/peter/src/pykythe/pykythe/ast_raw.py#L81
// does a redirect to:
//   http://localhost:999/static/src_browser.html?corpus=...
// and we depend on the server to handle redirections transparently,
// per the HTTP specs.
// A more complete (redirected) URL is:
//   http://localhost:9999/static/src_browser.html?corpus=CORPUS&root=ROOT&path=tmp%2Fpykythe_test%2FSUBST%2Fhome%2Fpeter%2Fsrc%2Fpykythe%2Ftest_data%2Fc3_a.py#L40

// All the IDs have defined prefixes (e.g., filetree_prefix, src_prefix, 'L'),
// or a few from <div>s (file_nav, src, xref).
// To check that all the IDs in the page are as expected:
//     Array.from(document.querySelectorAll('[id]')).sort();

// SourceItem class encapsulates corpus/root/path/lineno/hilite into a
// single object (lineno is optional and defaults to 1; hilite is
// semantic object and defaults to null), with some convenience
// methods and constructors.
class SourceItem {
    constructor(corpus, root, path, lineno, hilite) {
        this.corpus = corpus || '';
        this.root = root || '';
        this.path = path || '';
        this.lineno = lineno || 1;
        this.hilite = hilite || null;
    }

    static newFromJSON(json) {
        return new SourceItem(json.corpus, json.root, json.path, json.lineno);
    }

    // Split the path into components path0, path1, ... pathN; and return
    // [corpus, root, path0, path1, path2, ... pathN]
    pathItems() {
        // ''.split('') == [''] but we want []
        return [this.corpus, this.root]
            .concat((this.path === '') ? [] : this.path.split('/'));
    }

    combinedFilePath() {
        return this.corpus + ' / ' + this.root + ' / ' + this.path;
    }

    toString() {
        return combinedFilePath + '#' + linenoId(this.lineno) +
            (this.hilite ? '#hilite=' + this.hilite : '');
    }
}

// Global mapping anchor signature to anchor signatures that is used to
// highlight when the mouse moves over an anchor. An anchor always maps
// to itself, plus possibly some more.
var g_anchor_to_anchors = {};

// Global mapping anchor signature to semantic signatures.
var g_anchor_to_semantics = {};

// Global mapping semantic signature to anchor signatures.
var g_semantic_to_anchors = {};

// Global tree of dir/file entries - set by dynamic load from server
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
// Fetches the file tree from the server and schedules continuation
// that displays the file selector dropdown(s).
async function renderPage() {
    // https://developers.google.com/web/updates/2016/01/urlsearchparams
    const params = new URLSearchParams(location.search);
    const lineno = location.hash ? idLineno(location.hash) : 1;
    initDrag('initial');
    window.onresize = () => initDrag('resize'); // TODO: not needed?
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

function initDrag(debug_reason) {
    let src_elem = document.getElementById('src');
    let splitter_elem = document.getElementById('splitter');
    let xref_elem = document.getElementById('xref');

    // See diagram https://developer.mozilla.org/en-US/docs/Web/API/CSS_Object_Model/Determining_the_dimensions_of_elements
    // This code derived from https://stackoverflow.com/questions/12194469/best-way-to-do-a-split-pane-in-html/52536726#52536726
    var md = null; // remember mouse down info

    splitter_elem.onmousedown = (e) => {
        md = {e,
              splitterTop: splitter_elem.offsetTop,
              srcHeight:   src_elem.offsetHeight,
              xrefHeight:  xref_elem.offsetHeight,
              xrefTop:     xref_elem.offsetTop,
             };

        document.onmousemove = (e) => {
            var delta = {x: e.clientX - md.e.clientX,
                         y: e.clientY - md.e.clientY};

            // Prevent negative-sized elements
            delta.y = Math.min(Math.max(delta.y, -md.srcHeight), md.xrefHeight);

            src_elem.style.height = (md.srcHeight + delta.y) + 'px';
            splitter_elem.style.top = (md.splitterTop + delta.y) + 'px';
            xref_elem.style.height = (md.xrefHeight - delta.y) + 'px';
            // TODO: if grid layout, don't need to modify xref_elem.style.top?
            xref_elem.style.top = (md.xrefTop + delta.y) + 'px';
        }

        document.onmouseup = () => {
            document.onmousemove = document.onmouseup = null;
        }
    }
}

// Callback from server fetch of the file navigation tree
// Displays the file selector dropdown(s).
function setFileTree(file_tree_from_server, source_item) {
    g_file_tree = file_tree_from_server;
    displayFileTree(source_item);
}

// Display file tree (id='file_nav')
async function displayFileTree(source_item) {
    deleteAllChildren(fileNavElement());
    await displayFileTreeItems(g_file_tree, 0, source_item.pathItems(),
                               source_item.lineno, source_item.hilite);
}

const line_in_menu = '—————'; // multiple Unicode em-dashes

const filetree_prefix = 'nav_sel-'; // prefix for IDs in file tree SELECT dropdowns
const src_prefix = 'sig-'; // prefix for IDs that are signatures in the srce

// Recursive function for displaying the remaining file tree
// navigation items, starting from item_i (0-indexed).
// The path_items are used to "pre-select" items in the drop-downs.
// (path_item[0] is corpus; path_item[1] is root; path_item[2..] are
// the actual file path.)
async function displayFileTreeItems(file_tree_nodes, item_i, path_items, lineno, hilite) {
    let dropdown = makeFileTreeDropdown(file_tree_nodes, item_i, path_items);
    const selected_node = file_tree_nodes[dropdown.selectedIndex];
    fileNavElement().appendChild(dropdown);
    if (path_items.length > 0) {
        if (selected_node.type == 'dir') {
            await displayFileTreeItems(selected_node.children, item_i + 1, path_items.slice(1),
                                       lineno, hilite);
        } else if (selected_node.type == 'file') {
            await displayNewSrcFile(new SourceItem(
                selected_node.corpus, selected_node.root, selected_node.path, lineno, hilite));
        } else {
            return alert('Bad file_tree_nodes.type: ' + selected_node[0].type);
        }
    } else if (file_tree_nodes.length == 1) {
        if (file_tree_nodes[0].type == 'dir') {
            await displayFileTreeItems(file_tree_nodes[0].children, item_i + 1, [], lineno, hilite);
        } else if (file_tree_nodes[0].type == 'file') {
            await displayNewSrcFile(new SourceItem(
                selected_node.corpus, selected_node.root, selected_node.path, lineno, hilite));
        } else {
            return alert('Bad file_tree_nodes.type: ' + file_tree_nodes[0].type);
        }
    }
}

function makeFileTreeDropdown(file_tree_nodes, item_i, path_items) {
    let dropdown = createDropdownSelect();
    if (path_items.length == 0) {
        if (file_tree_nodes.length > 1) {
            addDropdownOption(dropdown,
                              {type: 'dir',name: line_in_menu,
                               corpus: null, root: null, path: null});
        }
        for (const tree_item of file_tree_nodes) {
            addDropdownOption(dropdown, tree_item);
        }
        dropdown.selectedIndex = 0;
    } else {
        for (const [i, tree_item] of file_tree_nodes.entries()) {
            // TODO: make directories display differently
            addDropdownOption(dropdown, tree_item);
            if (tree_item.name == path_items[0]) {
                dropdown.selectedIndex = i;
            }
        }
    }
    return dropdown;
}

// Create a dropdown (<SELECT ...>) element for file selection.
function createDropdownSelect() {
    // There's no way to catch the "onClick" for an individual
    // dropdown item; only the dropdown as a whole, which gets the ID
    // of the selected item in e.currentTarget.selectedIndex.
    let dropdown = document.createElement('select');
    dropdown.setAttribute('class', path_type_to_class['dir']);
    dropdown.onclick = async function(e) {
        e.preventDefault();
        const  t = e.currentTarget;
        if (t.selectedIndex == null) { return; } // ==, not ===: also matches undefined
        const  t_selected = t[t.selectedIndex];
        if (! t_selected || ! t_selected.id) { return; }
        console.assert(t_selected.id.startsWith(filetree_prefix), 'Invalid filetree_prefix', t_selected.id, 'should start with:', filetree_prefix);
        const id_parsed = JSON.parse(t_selected.id.substr(filetree_prefix.length));
        const tree_item = new SourceItem(id_parsed.corpus, id_parsed.root, id_parsed.path);
        displayFileTree(tree_item);
    };
    return dropdown;
}

// Add an <OPTION ...> element to a dropdown for file selection.
function addDropdownOption(dropdown, tree_item) {
    let option = document.createElement('option');
    option.setAttribute('class', path_type_to_class[tree_item.type]);
    option.text = tree_item.name;
    if (tree_item.path) {
        option.id = filetree_prefix + JSON.stringify(
            {corpus: tree_item.corpus, root: tree_item.root, path: tree_item.path});
    }
    dropdown.add(option);
}

// Update the displayed URL, to allow navigating back to this page.
// See setXrefEdgeLinkItem() for how the URL is created.
function updateBrowserUrl(source_item) {
    // TODO: is there a better choice for the 1st arg to replaceState?
    history.replaceState(
        {}, 'Pykythe Browser',
        location.origin + location.pathname +
            '?corpus=' + encodeURIComponent(source_item.corpus) +
            '&root=' + encodeURIComponent(source_item.root) +
            '&path=' + encodeURIComponent(source_item.path) +
            (source_item.hilite ? '&hilite=' + encodeURIComponent(source_item.hilite) : '') +
            '#L' + encodeURIComponent(source_item.lineno));
}

// Callback from file tree navigation click, to load a file into the
// fileNavElement() via displaySrcContents.
async function displayNewSrcFile(source_item) {
    updateBrowserUrl(source_item);
    let progress = document.createElement('span');
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
    let table = document.createElement('table');
    table.setAttribute('class', 'src_table');
    for (const [line_key, line_parts] of color_data.lines.entries()) {
        let row = table.insertRow();
        let td1 = row.insertCell();
        td1.setAttribute('class', 'src_lineno');
        td1.id = linenoId(line_key + 1);  // TODO: Fix the 0-origin line_key (the source display is 1-origin)
        let td2 = row.insertCell();
        td2.setAttribute('class', 'src_line');
        let txt_span = document.createElement('span');
        if (line_parts.length) {
            td1.appendChild(document.createTextNode(line_parts[0].lineno));
            srcLineTextAddHoverable(line_parts, txt_span, source_item);
        } else {
            txt_span.innerHTML = '&nbsp;';
        }
        td2.appendChild(txt_span);
    }
    replaceChildWith('src', table);
    scrollIntoViewAndMark(source_item.lineno, source_item.hilite, source_item);
    fileNavElement().lastChild.remove(); // Remove status message
}

// Simplified source display (no active links)
// See also srcLineTextAddHoverable()
function srcLineTextSimple(line_parts, txt_span, data_semantics) {
    for (const line_part of line_parts) {
        let span = document.createElement('span');
        span.setAttribute('class', token_css_color_class[line_part.token_color]);
        // TODO: the following doesn't hilite - why?  DO NOT SUBMIT FIXME
        if (is_token_name[line_part.token_color] &&
            (data_semantics || []).indexOf(g_anchor_to_semantics[line_part.signature]) >= 0) {
            span.classList.add('src_hilite');
        }
        span.innerHTML = sanitizeText(line_part.value);
        txt_span.appendChild(span)
    }
}

// Display a single line of a source display
function srcLineTextAddHoverable(line_parts, txt_span, source_item) {
    for (const line_part of line_parts) {
        let span = document.createElement('span');
        span.setAttribute('class', token_css_color_class[line_part.token_color]);
        span.innerHTML = sanitizeText(line_part.value);
        if (is_token_name[line_part.token_color] && line_part.signature) {
            span.id = src_prefix + line_part.signature;
            span.onmouseover = async function(e) { // e is MouseEvent
                e.preventDefault();
                mouseoverAnchor(e.currentTarget, 'add', 'src_hover', source_item);
            };
            span.onmouseleave = async function(e) { // e is MouseEvent
                e.preventDefault();
                mouseoverAnchor(e.currentTarget, 'remove', 'src_hover', source_item);
            };
            span.onclick = async function(e) { // e is MouseEvent
                e.preventDefault();
                await clickAnchor(e.currentTarget, source_item);
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
    console.assert(mouse_target.id.startsWith(src_prefix), 'Invalid src_prefix', mouse_target.id, 'should start with:', src_prefix);
    const signature = mouse_target.id.substr(src_prefix.length);
    mouseoverHilite(class_action, class_id,
                    g_anchor_to_anchors[signature], mouse_target.id);
}

// Highlight on/off (class_action={'add','remove}) on an anchor.
function mouseoverHilite(class_action, class_id, anchors, debug_item) {
    for (const anchor of anchors || []) {
        const sig = document.getElementById(src_prefix + anchor);
        if (sig) {
            sig.classList[class_action](class_id); // sig.classList.{add,remove}(class_id)
        } else {
            if (class_action === 'add') {
                console.trace('No edge for anchor', anchor, debug_item);
                // alert('No edge for ' + debug_item);
            }
        }
    }
}

// Callback for a click on a token (anchor) in the source display
async function clickAnchor(mouse_target, source_item) {
    console.assert(mouse_target.id.startsWith(src_prefix), 'Invalid src_prefix', mouse_target.id, 'should start with:', src_prefix);
    const signature = mouse_target.id.substr(src_prefix.length);
    await fetchFromServer(
        {anchor_xref: {signature: signature,
                       corpus: source_item.corpus,
                       root: source_item.root,
                       path: source_item.path,
                       language: 'python'}},  // DO NOT SUBMIT - don't hard-code language
        data => setXref(source_item, mouse_target.id, data));
}

// Callback from getting Kythe facts for a token (anchor) click
function setXref(source_item, signature, data) {
    // expected out-edges for anchor: defines, defines/binding, ref, ref/call
    document.getElementById('xref').innerHTML = sanitizeText(
        'Getting Kythe links for ' + source_item.combinedFilePath() + ' anchor:' + signature + ' ...');

    let table = document.createElement('table');
    setXrefItemHeader(table, signature, data);
    setXrefNodeValues(table, data);
    setXrefEdgeLinks(table, data, signature, source_item);
    setXrefBottom(table);
}

// In the xref area, display the item signature as a header
function setXrefItemHeader(table, signature, data) {
    table.setAttribute('class', 'src_table');
    let row_cell = tableInsertRowCell(table);
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

// In the xref area, add the xref links
function setXrefEdgeLinks(table, data, signature, source_item) {
    for (const edge_links of data.edge_links) {
        setXrefEdgeLinkHead(table, edge_links);
        for (const path_link of edge_links.links) {
            setXrefEdgeLinkItem(table, path_link, data, signature, source_item);
        }
    }
}

// In the xref area, a header for the links
function setXrefEdgeLinkHead(table, edge_links) {
    let row_cell = tableInsertRowCell(table);
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

// In the xref area, add a single xref item
function setXrefEdgeLinkItem(table, path_link, data, signature, source_item) {
    {
        let row_cell = tableInsertRowCell(table);
        // DO NOT SUBMIT - use a CSS class for '<b><i>':
        cellHTML(row_cell,
                 '<b><i>' + sanitizeText(path_link.path) + '</i></b>');
    }
    for (const link_line of path_link.lines) {
        let row_cell = tableInsertRowCell(table);
        const lineno_span = row_cell.appendChild(document.createElement('a'));
        lineno_span.title = link_line.path + ':' + link_line.lineno;
        const semantic = g_anchor_to_semantics[signature];
        const href = location.origin + location.pathname +
              '?corpus=' + encodeURIComponent(link_line.corpus) +
              '&root=' + encodeURIComponent(link_line.root) +
              '&path=' + encodeURIComponent(link_line.path) +
              (semantic ? '&hilite=' + encodeURIComponent(semantic) : '') +
              '#' + encodeURIComponent(linenoId(link_line.lineno));
        lineno_span.href = href;
        lineno_span.innerHTML = '<b><i>' + link_line.lineno + ':&nbsp;</i></b>';  // DO NOT SUBMIT - CSS class, rowspan
        let txt_span = row_cell.appendChild(document.createElement('a'));
        txt_span.title = link_line.path + ':' + link_line.lineno;
        txt_span.href = href;
        // For the case of same file, the href-fetch doesn't cause the
        // lineno-handling stuff to be executed, so do it ourselves:
        if (link_line.corpus == source_item.corpus &&
            link_line.root == source_item.root &&
            link_line.path == source_item.path) {
            txt_span.onclick = async function (e) {
                e.preventDefault();  // Prevent standard behavior of scrollIntoView({block: 'start'})
                scrollIntoViewAndMark(link_line.lineno, semantic, link_line);
                updateBrowserUrl(link_line);
            }
        }
        srcLineTextSimple(link_line.line, txt_span, data.semantics);
    }
}

function setXrefBottom(table) {
    let row_cell = tableInsertRowCell(table);
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

// Make a string that combines the number and the appropriate
// singular/plural noun.
function singularPlural(number, singular, plural) {
    if (number == 0) {
        return 'zero ' + plural;
    } else if (number == 1) {
        return 'one ' + singular;
    } else {
        return number + ' ' + plural;
    }
}
// There shouldn't be a need for .replace(/ /g, '&nbsp;') if CSS
// has white-space:pre ... but by experiment, it's needed.
const sanitize_re = /[&<>\n "'`/]/g;
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
    let elem = document.getElementById(id);
    // This should suffice, but let's instead delete
    // the children and replace:
    //     elem.replaceChild(new_child, elem.firstChild);
    deleteAllChildren(elem);
    elem.appendChild(new_child);
}

// Clear an element.
// Recursively removes chidren, even though probably not needed
function deleteAllChildren(elem) {
    while (elem.firstChild) {
        deleteAllChildren(elem.firstChild);
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

// Ensure that a line is visible.
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
            console.trace('NO LINE:', linenoId(lineno), debug_item);
        }
    } else {
        console.trace('NO LINENO', debug_item);
    }
    mouseoverHilite('add', 'src_hover', g_semantic_to_anchors[hilite], hilite);
}

// Convert an ID or #ID (from location.hash) to a lineno
function idLineno(id) {
    let lineno;
    if (id.substr(0, 1) == 'L') {
        lineno = parseInt(id.substr(1));
    } else if (id.substr(0, 2) == '#L') {  // From location.hash
        lineno = parseInt(id.substr(2));
    } else {
        lineno = NaN;
    }
    if (isNaN(lineno)) {
        console.trace('Invalid lineno ID: "' + id + '"');
        lineno = 1;
    }
    return lineno;
}

// End of file
