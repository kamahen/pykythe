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

// SourceItem class encapsulates corpus/root/path/lineno into
// a single object (lineno is optional and defaults to 1),
// with some convenience methods and constructors.
class SourceItem {
    constructor(corpus, root, path, lineno) {
        // TODO: escape '/' in corpus, root
        this.corpus = corpus || '';
        this.root = root || '';
        this.path = path || '';
        this.lineno = lineno || 1;
    }

    static newFromCombined(corpus_root_path, lineno) {
        // TODO: escape '/' in corpus, root
        const corpus_root_path_split = corpus_root_path.split('/');
        return new SourceItem(corpus_root_path_split[0], // corpus
                              corpus_root_path_split[1], // root
                              corpus_root_path_split.slice(2).join('/'), // path
                              lineno);
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
        return combinedFilePath + '#' + lineno_id(this.lineno);
    }
}


// global 'g_anchor_edges' gets
//    {signature:str, edge:str, target:{corpus:_,root:_,path:_,language:_,signature:_}}
//    items (see color_data.lines[*].edges)
var g_anchor_edges = [];

// tree of dir/file entries - set by dynamic load from server
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
    const lineno = location.hash ? id_lineno(location.hash) : 1;
    await fetchFromServer(
        {src_file_tree: ''},
        file_tree_from_server => setFileTree(
            file_tree_from_server,
            new SourceItem(
                params.get('corpus'),
                params.get('root'),
                params.get('path'),
                lineno)));
}

// Callback from server fetch of the file navigation tree
function setFileTree(file_tree_from_server, source_item) {
    g_file_tree = file_tree_from_server;
    displayFileTree(source_item);
}

// Display file tree (id='file_nav')
async function displayFileTree(source_item) {
    const path_items = source_item.pathItems();
    var tree = file_nav_element();
    while (tree.firstChild) {
        tree.firstChild.remove();
    }
    await displayFileTreeItems(0, path_items, g_file_tree, source_item.lineno);
}

// Recursive function for displaying the remaining file tree
// navigation items, starting from item_i (0-indexed).
// The path_items are used to "pre-select" items in the drop-downs.
async function displayFileTreeItems(item_i, path_items, file_tree_nodes, lineno) {
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
    file_nav_element().appendChild(dropdown);
    const selected_node = file_tree_nodes[dropdown.selectedIndex];

    if (path_items.length > 0) {
        if (selected_node.type == 'dir') {
            await displayFileTreeItems(item_i + 1, path_items.slice(1),
                                       selected_node.children, lineno);
        } else if (selected_node.type == 'file') {
            await displayNewSrcFile(SourceItem.newFromCombined(selected_node.path, lineno));
        } else {
            return alert('Bad file_tree_nodes.type: ' + selected_node[0].type);
        }
    } else if (file_tree_nodes.length == 1) {
        if (file_tree_nodes[0].type == 'dir') {
            await displayFileTreeItems(item_i + 1, [], file_tree_nodes[0].children, lineno);
        } else if (file_tree_nodes[0].type == 'file') {
            await displayNewSrcFile(SourceItem.newFromCombined(selected_node.path, lineno));
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
// file_nav_element() via displaySrcContents.
async function displayNewSrcFile(source_item) {
    var progress = document.createElement('span');
    progress.innerHTML = '&nbsp;&nbsp;&nbsp;Fetching file ' +
        sanitizeText(source_item.combinedFilePath()) + ' ...';
    file_nav_element().appendChild(progress);
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
// TODO: remove the duplication of information (the result of refactoring)
function displaySrcContents(source_item, color_data) {
    console.assert(source_item.corpus === color_data.corpus &&
                   source_item.root === color_data.root &&
                   source_item.path === color_data.path,
                   "Mismatch source-item, color_data");
    file_nav_element().lastChild.innerHTML =
        'Rendering file ' + source_item.combinedFilePath() + '...';
    // console.log('DISPLAY_SRC: ' + JSON.stringify(color_data));  // DO NOT SUBMIT
    var table = document.createElement('table');
    table.setAttribute('class', 'src_table');
    for (const [line_key, line_parts] of color_data.lines.entries()) {
        var row = table.insertRow();
        var td1 = row.insertCell();
        td1.setAttribute('class', 'src_lineno');
        td1.id = lineno_id(line_key + 1);  // TODO: Fix the 0-origin line_key (the source display is 1-origin)
        var td2 = row.insertCell();
        td2.setAttribute('class', 'src_line');
        var txt_span = document.createElement('span');
        if (line_parts.length) {
            td1.appendChild(document.createTextNode(line_parts[0].lineno));
            srcLineText(line_parts, txt_span, source_item);
        } else {
            txt_span.innerHTML = '&nbsp;';
        }
        td2.appendChild(txt_span);
    }
    replaceChildWith('src', table);
    if (source_item.lineno) {
        const line_elem = document.getElementById(lineno_id(source_item.lineno));
        if (line_elem) {
            // TODO: ensure behavior is same as hash ('#' + lineno_id(source_item.lineno))
            // Choices are: 'start', 'center', 'end', 'nearest'
            line_elem.scrollIntoView({block: 'center', inline: 'start'});
            // TODO: highlight the source text as well as the line #
            line_elem.setAttribute('class', 'src_lineno_hilite');
        } else {
            console.log('NO LINE:', lineno_id(source_item.lineno), source_item);
        }
    } else {
        console.log('NO LINENO', source_item);
    }
    file_nav_element().lastChild.remove(); // Remove status message
}

// Simplified source display (no active links)
function srcLineTextSimple(txt_span, parts, highlight_semantic) {
    for (const part of parts) {
        var span = document.createElement('span');
        span.setAttribute('class', token_css_color_class[part.token_color]);
        if (is_token_name[part.token_color] && part.signature == highlight_semantic) {
            span.classList.add('src_hover');
        }
        span.innerHTML = sanitizeText(part.value);
        txt_span.appendChild(span)
    }
}

// Display a single line of a source display
function srcLineText(parts, txt_span, source_item) {
    for (const part of parts) {
        var span = document.createElement('span');
        span.setAttribute('class', token_css_color_class[part.token_color]);
        span.innerHTML = sanitizeText(part.value);
        if (is_token_name[part.token_color] && part.signature) {
            for (const p_edge of part.edges) {
                g_anchor_edges.push({signature: part.signature,
                                     edge: p_edge.edge,
                                     target: p_edge.target});
            }
            span.id = part.signature;  // DO NOT SUBMIT -- should have a known prefix on the id
            span.onmouseover = async function(e) { // e is MouseEvent
                mouseoverAnchor(e.currentTarget, 'add', 'src_hover');
                e.preventDefault();
            };
            span.onmouseleave = async function(e) { // e is MouseEvent
                mouseoverAnchor(e.currentTarget, 'remove', 'src_hover');
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
        } else if (is_token_name[part.token_color]) {
            console.assert(part.signature,
                           'SIGNATURE SHOULD NOT BE EMPTY',
                           part.token_color, part.signature);
        } else {
            console.assert(! part.signature,
                           'SIGNATURE SHOULD BE EMPTY',
                           part.token_color, part.signature);
        }
        txt_span.appendChild(span);
    }
}

// Do an action ('add' or 'remove') for an item in a class list
// - callback from mouseover/leave on a token (anchor) in the source display
function mouseoverAnchor(target, class_action, class_id) {
    for (const a_edge of anchor_target_edges(target.id)) {
        for (const t_a_edge of target_anchor_edges(a_edge.target)) {
            const edge = document.getElementById(t_a_edge.signature);
            if (edge) {
                edge.classList[class_action](class_id);
            } else {
                // TODO: this doesn't seem to happen consistently:
                // DO NOT SUBMIT - shouldn't happen - do we need to filter
                //                 for same source path?
                // *** can make it happen with test_data/c3a.py __bases__
                // *** but if click on builtins.py __bases__, doesn't happen
                // *** but then go to another file, it does happen
                console.log('No edge for ' + target.id + ' ' + t_a_edge.signature);
            }
        }
    }
}

// Callback for a click on a token (anchor) in the source display
async function clickAnchor(target, source_item) {
    console.log('CLICK ' + target.id + ' in ' + source_item.combinedFilePath());
    await fetchFromServer(
        {anchor_xref: {signature: target.id,
                       corpus: source_item.corpus,
                       root: source_item.root,
                       path: source_item.path,
                       language: 'python'}},  // DO NOT SUBMIT - don't hard-code language
        data => setXref(source_item, target.id, data));
}

// Callback from getting Kythe facts for a token (anchor) click
function setXref(source_item, signature, data) {
    // expected out-edges for anchor: defines, defines/binding, ref, ref/call

    document.getElementById('xref').innerHTML = 'Getting Kythe links for ' + source_item.combinedFilePath() + ' anchor:' + signature + ' ...';
    var table = document.createElement('table');
    table.setAttribute('class', 'src_table');  // DO NOT SUBMIT - should we have a new CSS class for this?
    var row_cell = tableInsertRowCell(table);
    // TODO: remove <i> using row_cell.settAttribute('class', some-other-class)
    if (data.semantics.length == 0) {
        row_cell.appendChild(document.createElement('span')).innerHTML + '<i>(no semantics)</i>';
    } else {
        row_cell.appendChild(document.createElement('span')).innerHTML = data.semantics.map(
            s => '<i>' + sanitizeText(s.signature) + '</i>').join('<br/>');
    }
    for (const nv of data.semantic_node_values) {
        // TODO: use class attributes:
        tableInsertRowCellHTML(table, '&nbsp;&nbsp;<b>' +
                               sanitizeText(nv.kind) +
                               '</b>:&nbsp;' +
                               sanitizeText(nv.value));
    }
    for (const edge_links of data.edge_links) {
        row_cell = tableInsertRowCell(table);
        row_cell.setAttribute('class', 'xref_head');
        cellHTML(row_cell,
                 sanitizeText(edge_links.edge + ' (' +
                              singular_plural(edge_links.links.length, 'file', 'files') +
                              ', ' +
                              singular_plural(
                                  edge_links.links.map(link => link.lines.length)
                                      .reduce((x,y)=>x+y,0), 'line', 'lines') +
                              ')'));
        for (const path_link of edge_links.links) {
            row_cell = tableInsertRowCell(table);
            // DO NOT SUBMIT - use a CSS class for '<b><i>':
            cellHTML(row_cell,
                     '<b><i>' + sanitizeText(path_link.path) + '</i></b>');
            for (const link_line of path_link.lines) {
                row_cell = tableInsertRowCell(table);
                const lineno_span = row_cell.appendChild(document.createElement('a'));
                const xref_title = sanitizeText('xref-title'); // DO NOT SUBMIT - addd semantic signature
                lineno_span.title = xref_title;
                // TODO - DO NOT SUBMIT
                //   The following href doesn't do what you expect when it refers to
                //   the source file that's currently being displayed. Need to check
                //   for same file and then invoke the scrollIntoView logic that's
                //   in displaySrceContents.
                const href = location.origin + location.pathname +
                      '?corpus=' + link_line.corpus +
                      '&root=' + link_line.corpus +
                      '&path=' + link_line.path +
                      '#' + lineno_id(link_line.lineno);
                lineno_span.href = href;
                lineno_span.innerHTML = '<b><i>' + link_line.lineno + ':&nbsp;</i></b>';  // DO NOT SUBMIT - CSS class, rowspan
                var txt_span = row_cell.appendChild(document.createElement('a'));
                txt_span.title = 'xref-title-src'); // DO NOT SUBMIT - addd semantic signature
                txt_span.href = href;
                srcLineTextSimple(txt_span, link_line.line, data.semantic);
            }
        }
    }
    row_cell = tableInsertRowCell(table);
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

function singular_plural(number, singular, plural) {
    if (number == 1) {
        return 'one ' + singular;
    } else {
        return number + ' ' + plural;
    }
}

// Sanitize a string, allowing tags to not cause problems
function sanitizeText(raw_str) {
    // There shouldn't be a need for .replace(/ /g, '&nbsp;') if CSS
    // has white-space:pre ... but by experiment, it's needed.
    // TODO: remove the '<br/>' insertion and put it into extract_color.pl.
    return (raw_str)
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&apos;')
        .replace(/\n/g, '<br/>')  // TODO: remove - not needed?
        .replace(/\s/g, '&nbsp;');  // TODO: add test for tabs in source
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

// Look up edges that match an anchor signature (click/mouseover target.id)
function anchor_target_edges(anchor_signature) {
    return g_anchor_edges
        .filter(edge => edge.signature === anchor_signature);
}

// Look up edges that match a corpus,root,path,signature,language
function target_anchor_edges(target) {
    return g_anchor_edges
        .filter(edge =>
            edge.target.signature === target.signature &&
                edge.target.corpus === target.corpus &&
                edge.target.root === target.root &&
                edge.target.path === target.path &&
                edge.target.language === target.language);
}

// Convenience function: get the 'file_nav' element
function file_nav_element() {
    return document.getElementById('file_nav');
}

// Convert a lineno to an ID
function lineno_id(lineno) {
    return 'L' + lineno;
}

// Convert an ID or #ID (from location.hash) to a lineno
function id_lineno(id) {
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
