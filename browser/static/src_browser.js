'use strict';

// Implementation of annotated source browser, using nodes defined by
// src_browser.html.

// http://localhost:9999/static/src_browser.html?corpus=CORPUS&root=ROOT&path=home/peter/src/pykythe/pykythe/ast_raw.py&line=81

// TODO: The code is inconsistent in whether it has corpus, root, path as
//       separate items or combined into 'corpus/root/path'.

// DO NOT SUBMIT -- make a class for corpus, root, path, lineno, highlight, ...
//   (scan for "lineno")

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
        return combinedFilePath + ':#L' + this.lineno;
    }
}


// global 'g_anchor_edges' gets
//    {signature:str, edge:str, target:{corpus,root,path,language,signature}
// items (see color_data.lines[*].edges)
var g_anchor_edges = [];

// TODO: use window.location.assign(window.location.assign + '?' + ...)

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
    '<STRING>':             'python_string',
    '<VAR_BINDING>':        'python_var_binding',
    '<VAR_BINDING_GLOBAL>': 'python_binding_global',
    '<VAR_REF>':            'python_var_ref',
    '<WHITESPACE>':         'python_whitespace', // for completeness
};

// Map the ast_color.Color.token_color enumeration to whether it's
// a colorable item (a "token") or not.
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

// Callback from <body onload="render_page();">
function renderPage() {
    // https://developers.google.com/web/updates/2016/01/urlsearchparams
    const params = new URLSearchParams(location.search);
    fetchFromServer({src_file_tree: ''},
                    file_tree_from_server => setFileTree(
                        file_tree_from_server,
                        new SourceItem(
                            params.get('corpus'),
                            params.get('root'),
                            params.get('path'),
                            params.get('line'))));
}

// Callback from fetchFromServer({src_file_tree: ''}),
// for displaying the file navigation tree
function setFileTree(file_tree_from_server, source_item) {
    g_file_tree = file_tree_from_server;
    displayFileTree(source_item);
}

// Display file tree (id='file_nav')
function displayFileTree(source_item) {
    const path_items = source_item.pathItems();
    var tree = file_nav_element();
    while (tree.firstChild) {
        tree.firstChild.remove();
    }
    displayFileTreeItems(0, path_items, g_file_tree, source_item.lineno);
}

// Recursive function for displaying the remaining file tree
// navigation items, starting from item_i (0-indexed).
// The path_items are used to "pre-select" items in the drop-downs.
function displayFileTreeItems(item_i, path_items, file_tree_nodes, lineno) {
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
        for (var i = 0; i < file_tree_nodes.length; i++) {
            // TODO: make directories display differently
            var tree_item = file_tree_nodes[i];
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
            displayFileTreeItems(item_i + 1, path_items.slice(1),
                                 selected_node.children, lineno);
        } else if (selected_node.type == 'file') {
            displayNewSrcFile(SourceItem.newFromCombined(selected_node.path, lineno));
        } else {
            return alert('Bad file_tree_nodes.type: ' + selected_node[0].type);
        }
    } else if (file_tree_nodes.length == 1) {
        if (file_tree_nodes[0].type == 'dir') {
            displayFileTreeItems(item_i + 1, [], file_tree_nodes[0].children, lineno);
        } else if (file_tree_nodes[0].type == 'file') {
            displayNewSrcFile(SourceItem.newFromCombined(selected_node.path, lineno));
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
    dropdown.onclick = function(e) {
        const  t = e.currentTarget;
        const  t_selected = t[t.selectedIndex];
        if (t_selected.id.startsWith('nav_sel-')) {
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
function displayNewSrcFile(source_item) {
    var progress = document.createElement('span');
    progress.innerHTML = '&nbsp;&nbsp;&nbsp;Fetching file ' +
        sanitizeText(source_item.combinedFilePath()) + ' ...';
    file_nav_element().appendChild(progress);
    // TODO: alert if fetch fails
    fetchFromServer(
        {src_browser_file: {corpus: source_item.corpus,
                            root: source_item.root,
                            path: source_item.path}},
        color_data => displaySrcContents(source_item, color_data));
}

// Callback from server fetch of a single source file (from displayNewSrcFile)
function displaySrcContents(source_item, color_data) {
    file_nav_element().lastChild.innerHTML =
        'Rendering file ' + source_item.combinedFilePath() + '...';
    var table = document.createElement('table');
    table.setAttribute('class', 'src_table');
    for (var line_key = 1; line_key <= color_data.lines.length; line_key++) {
        const line_parts = color_data.lines[line_key - 1];
        var row = table.insertRow();
        var td1 = row.insertCell();
        td1.setAttribute('class', 'src_lineno');
        td1.id = 'L' + line_key;
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
        var line_elem = document.getElementById('L' + source_item.lineno);
        if (line_elem) {
            // Choices are: 'start', 'center', 'end', 'nearest'
            line_elem.scrollIntoView({block: 'center'});
        }
    }
    file_nav_element().lastChild.remove(); // Remove status message
}

// Display a single part of a source display
function srcLineText(parts, txt_span, source_item) {
    for (const part of parts) {
        var span = document.createElement('span');
        span.setAttribute('class', token_css_color_class[part.token_color]);
        span.innerHTML = sanitizeText(part.value);
        if (is_token_name[part.token_color]) {
            for (const p_edge of part.edges) {
                g_anchor_edges.push({signature: part.signature,
                                     edge: p_edge.edge,
                                     target: p_edge.target});
            }
            span.id = part.signature;
            span.onmouseover = function(e) { // e is MouseEvent
                mouseoverAnchor(e.currentTarget, 'add', 'src_hover');
                e.preventDefault();
            };
            span.onmouseleave = function(e) { // e is MouseEvent
                mouseoverAnchor(e.currentTarget, 'remove', 'src_hover');
                e.preventDefault();
            };
            span.onclick = function(e) { // e is MouseEvent
                clickAnchor(e.currentTarget, source_item);
                e.preventDefault();
            };
            // TODO: handle right-click
            // span.oncontextmenu = function(e) { // e is MouseEvent
            //     e.preventDefault();
            // };
            txt_span.appendChild(span);
        } else {
            txt_span.appendChild(span);
        }
    }
}

// Do an action ('add' or 'remove') for an item in a class list
// - callback from mouseover/leave on a token (anchor) in the source display
function mouseoverAnchor(target, class_action, class_id) {
    for (const a_edge of anchor_target_edges(target.id)) {
        for (const t_a_edge of target_anchor_edges(a_edge.target)) {
            var edge = document.getElementById(t_a_edge.signature);
            if (edge) {
                edge.classList[class_action](class_id);
            } else {
                // TODO: this doesn't seem to happen consistently:
                // DO NOT SUBMIT - shouldn't happen - do we need to filter
                //                 for same source path?
                console.log('No edge for ' + target.id + ' ' + t_a_edge.signature);
            }
        }
    }
}

// Callback for a click on a token (anchor) in the source display
function clickAnchor(target, source_item) {
    // console.log('CLICK ' + target.id + ' in ' + source_item.combinedFilePath());
    fetchFromServer({anchor: {signature: target.id,
                              corpus: source_item.corpus,
                              root: source_item.root,
                              path: source_item.path,
                              language: 'python'}},  // DO NOT SUBMIT - don't hard-code language
                    data => setXref(source_item, target.id, data));
}

// Callback from getting Kythe facts for a token (anchor) click
function setXref(source_item, signature, data) {
    // expected out-edges for anchor: defines, defines/binding, ref, ref/call
    // console.log('SET_XREF: ' + JSON.stringify(data));
    document.getElementById('xref').innerHTML = 'Getting Kythe links for ' + source_item.combinedFilePath() + ' anchor:' + signature + ' ...';
    var table = document.createElement('table');
    table.setAttribute('class', 'src_table');  // DO NOT SUBMIT - should we have a new class for this?
    tableInsertRowText(table, data.semantic.signature);
    for (const nv of data.semantic_node_values) {
        // TODO: this sanitizes the text; might want to use something different:
        tableInsertRowText(table, '  ' + nv.kind +': ' + nv.value);
    }
    for (const edge_links of data.edge_links) {
        tableInsertRowText(table, edge_links.edge);
        for (const link of edge_links.links) {
            tableInsertRowText(table, link.signature + ' ' + link.path);
        }
    }
    replaceChildWith('xref', table);
}

function tableInsertRowText(table, text) {
    table.insertRow().insertCell().appendChild(document.createTextNode(text));
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
function fetchFromServer(request, callback) {
    // callback should take a single arg, the response from the server,
    // e.g.: json_data => set_corpus_root_path_filename(json_data))
    fetch('/json',
          {method: 'POST',
           headers: {'Content-Type': 'application/json'},
           body: JSON.stringify(request),
           mode: 'cors',                  // Don't need?
           cache: 'no-cache',             // Don't need?
           credentials: 'same-origin',    // Don't need?
           redirect: 'follow',            // Don't need?
           referrerPolicy: 'no-referrer', // Don't need?
          })
        .then(response => response.json())
        .then(callback)
        // .catch(err => {  // TODO: move this to before the callback?
        //     console.log('fetch ' + request + ': ' + err)
        // })
    ;
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

// End of file
