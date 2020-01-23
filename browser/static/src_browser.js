'use strict';

// Implementation of annotated source browser, using nodes defined by
// src_browser.html.

// TODO: The code is inconsistent in whether it has corpus, root, path as
//       separate items or combined into 'corpus/root/path'.

// global 'g_anchor_edges' gets {signature:str, edge:str, target:{corpus,root,path,language,signature} items
// (see color_data.lines[line_key].edges)
var g_anchor_edges = [];

// TODO: use window.location.assign(window.location.assign + '?' + ...)

// list of corpus,root,path tuples - set by dynamic load from server
// TODO: should become a dict
var g_corpus_root_path_filename = null;

// tree of dir/file entries - set by dynamic load from server
var g_file_tree = null;

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
    '<ARG_KEYWORD>':        false, // for now
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

// Callback from <body onload=intialize();">
function initialize() {
    // https://developers.google.com/web/updates/2016/01/urlsearchparams
    console.log('QUERY: ' + window.location.search);
    const params = new URLSearchParams(location.search);
    fetch_from_server({fetch: 'FILETREE.json'},
                      data => set_file_tree(data, params.get('file') || ''));
    fetch_from_server({fetch: 'FILES.json'},
                      data => set_corpus_root_path_filename(data));
}

// Callback from file tree navigation click, to load a file into the
// file_nav_element() via set_src_txt_impl.
function load_new_file(corpus_root_path) {
    const corpus_root_path_split = corpus_root_path.split('/');
    const corpus = corpus_root_path_split[0];
    const root = corpus_root_path_split[1];
    const path = corpus_root_path_split.slice(2).join('/');
    const src_browser_file = find_file(corpus, root, path);
    if (!src_browser_file) {
        window.alert("Can't load " + path);
    } else {
        var progress = document.createElement('span');
        progress.innerHTML = '&nbsp;&nbsp;&nbsp;Fetching file ' + sanitize_text(corpus_root_path) + ' ...';
        file_nav_element().appendChild(progress);
        fetch_from_server({fetch: src_browser_file},
                          data => set_src_txt_impl(corpus, root, path, data));
    }
}

// Callback from loading FILETREE.json, for displaying the file navigation tree
function set_file_tree(file_tree_from_server, initial_path) {
    // path starts with 'corpus/root/...'
    // DO NOT SUBMIT -- shouldn't do the double parse
    g_file_tree = JSON.parse(file_tree_from_server.contents);
    display_file_tree(initial_path);
}

// Convenience function: get the 'file_nav' element
function file_nav_element() {
    return document.getElementById('file_nav');
}

// Display file tree (id='file_nav')
function display_file_tree(path) {
    const path_items = (path === '') ? [] : path.split('/');
    var tree = file_nav_element();
    while (tree.firstChild) {
        tree.firstChild.remove();
    }
    display_file_tree_items(0, path_items, g_file_tree);
}

// Recursive function for displaying the remaining file tree
// navigation items, starting from item_i (0-indexed).
function display_file_tree_items(item_i, path_items, file_tree_nodes) {
    // file_tree_nodes is a list of 'file' or 'dir' items

    var dropdown = create_dropdown(item_i);
    if (path_items.length == 0) {
        if (file_tree_nodes.length > 1) {
            add_dropdown_option(dropdown, '-----', 'nav_sel-' + file_tree_nodes.path);
        }
        for (const tree_item of file_tree_nodes) {
            add_dropdown_option(dropdown, tree_item.name, 'nav_sel-' + tree_item.path);
        }
        dropdown.selectedIndex = 0;
    } else {
        for (var i = 0; i < file_tree_nodes.length; i++) {
            var tree_item = file_tree_nodes[i];
            add_dropdown_option(dropdown, tree_item.name, 'nav_sel-' + tree_item.path);
            if (tree_item.name == path_items[0]) {
                dropdown.selectedIndex = i;
            }
        }
    }
    file_nav_element().appendChild(dropdown);
    const selected_node = file_tree_nodes[dropdown.selectedIndex];

    if (path_items.length > 0) {
        if (selected_node.type == 'dir') {
            display_file_tree_items(item_i + 1, path_items.slice(1),
                                    selected_node.children);
        } else if (selected_node.type == 'file') {
            load_new_file(selected_node.path);
        } else {
            return alert('Bad file_tree_nodes.type: ' + selected_node[0].type);
        }
    } else if (file_tree_nodes.length == 1) {
        if (file_tree_nodes[0].type == 'dir') {
            display_file_tree_items(item_i + 1, [], file_tree_nodes[0].children);
        } else if (file_tree_nodes[0].type == 'file') {
            load_new_file(selected_node.path);
        } else {
            return alert('Bad file_tree_nodes.type: ' + file_tree_nodes[0].type);
        }
    }
}

// Create a dropdown (<SELECT ...>) element
function create_dropdown(i) {
    var dropdown = document.createElement('select');
    dropdown.setAttribute('class', 'file_nav_sel');
    // dropdown.id = 'path-' + i;
    dropdown.onclick = function(e) {
        const  t = e.currentTarget;
        const  t_selected = t[t.selectedIndex];
        if (t_selected.id.startsWith('nav_sel-')) {
            display_file_tree(t_selected.id.substr('nav_sel-'.length));
        }
        e.preventDefault();
    };
    return dropdown;
}

// Add an <OPTION ...> element to a dropdown
function add_dropdown_option(dropdown, text, id) {
    var option = document.createElement('option');
    option.setAttribute('class', 'file_nav_sel');
    option.text = text;
    option.id = id;
    dropdown.add(option);
}

// Callback from fetching FILES.json
function set_corpus_root_path_filename(corpus_root_path_filename_from_server) {
    // DO NOT SUBMIT -- shouldn't do the double parse
    g_corpus_root_path_filename = JSON.parse(corpus_root_path_filename_from_server.contents);
}

// Lookup a file path, getting the name of a file to request from the server
// Returns null on failure.
function find_file(corpus, root, path) {
    for (const fn of g_corpus_root_path_filename) {
        if (fn.corpus === corpus &&
            fn.root === root &&
            fn.path === path) {
            return fn.filename;
        }
    }
    return null;
}

// Callback from server fetch of a single source file (from load_new_file)
function set_src_txt_impl(corpus, root, path, color_data_str) {
    file_nav_element().lastChild.innerHTML = 'Rendering file ' + corpus + '/' + root + '/' + path + '...';
    const color_data = JSON.parse(color_data_str.contents);
    var table = document.createElement('table');
    table.setAttribute('class', 'src_table');
    for (const line_key of color_data.line_keys) {
        const line_parts = color_data.lines[line_key];
        var row = table.insertRow();
        var td1 = row.insertCell();
        td1.setAttribute('class', 'src_lineno');
        td1.setAttribute('id', line_key.replace(/^0+/g, ''));
        var td2 = row.insertCell();
        td2.setAttribute('class', 'src_line');
        var txt_span = document.createElement('span');
        if (line_parts.length) {
            td1.appendChild(document.createTextNode(line_parts[0].lineno));
            src_line_txt(line_parts, txt_span, corpus, root, path);
        } else {
            txt_span.innerHTML = '&nbsp;';
        }
        td2.appendChild(txt_span);
    }
    replace_child_with('src', table);
    file_nav_element().lastChild.remove();
}

// Do an action ('add' or 'remove') for an item in a class list
// - callback from mouseover/leave on a token (anchor) in the source display
function do_for_signature(target, class_action, class_id) {
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
function click_signature(target, corpus, root, path) {
    console.log('CLICK ' + target.id + ' in ' + corpus + ':' + root + ':' + path);
    fetch_from_server({anchor: {signature: target.id,
                                corpus: corpus, root: root,
                                path: path,
                                language: 'python'}},  // DO NOT SUBMIT - don't hard-code language
                      data => set_xref(corpus, root, path, target.id, data));
}

// Callback from getting Kythe facts for a token (anchor) click
function set_xref(corpus, root, path, signature, data) {
    // expected out-edges for anchor: defines, defines/binding, ref, ref/call
    // console.log('SET_XREF: ' + JSON.stringify(data));
    document.getElementById('xref').innerHTML = 'Getting Kythe links for ' + corpus + '/' + root + '/' + path + ' anchor:' + signature + ' ...';
    var table = document.createElement('table');
    table.setAttribute('class', 'src_table');  // DO NOT SUBMIT - should we have a new class for this?
    table_insert_row_text(table, data.semantic.signature);
    for (const nv of data.semantic_node_values) {
        // TODO: this sanitizes the text; might want to use something different:
        table_insert_row_text(table, '  ' + nv.kind +': ' + nv.value);
    }
    for (const edge_links of data.edge_links) {
        table_insert_row_text(table, edge_links.edge);
        for (const link of edge_links.links) {
            table_insert_row_text(table, link.signature + ' ' + link.path);
        }
    }
    replace_child_with('xref', table);
}

function table_insert_row_text(table, text) {
    table.insertRow().insertCell().appendChild(document.createTextNode(text));
}

// Display a single part of a source display
function src_line_txt(parts, txt_span, corpus, root, path) {
    for (const part of parts) {
        var span = document.createElement('span');
        span.setAttribute('class', token_css_color_class[part.token_color]);
        span.innerHTML = sanitize_text(part.value);
        if (is_token_name[part.token_color]) {
            for (const p_edge of part.edges) {
                g_anchor_edges.push({signature: part.signature,
                                     edge: p_edge.edge,
                                     target: p_edge.target});
            }
            span.id = part.signature;
            span.onmouseover = function(e) { // e is MouseEvent
                do_for_signature(e.currentTarget, 'add', 'src_hover');
                e.preventDefault();
            };
            span.onmouseleave = function(e) { // e is MouseEvent
                do_for_signature(e.currentTarget, 'remove', 'src_hover');
                e.preventDefault();
            };
            span.onclick = function(e) { // e is MouseEvent
                click_signature(e.currentTarget, corpus, root, path);
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

// Sanitize a string, allowing tags to not cause problems
function sanitize_text(raw_str) {
    // There shouldn't be a need for .replace(/ /g, '&nbsp;') if CSS
    // has white-space:pre ... but by experiment, it's needed.
    // TODO: remove the '<br/>' insertion and put it into extract_color.pl.
    return (raw_str)
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&apos;')
        .replace(/\n/g, '<br/>')
        .replace(/\s/g, '&nbsp;');  // TODO: test tabs in source
}

// Send a request to the server and schedule a callback.
function fetch_from_server(request, callback) {
    // callback should take a single arg, the response from the server,
    // e.g.: json_data => set_corpus_root_path_filename(json_data))
    console.log('FETCH ' + JSON.stringify(request));
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
    // DO NOT SUBMIT - unused?
    return '.' + id.replace( /(:|\.|\[|\]|,|=|@|<|>)/g, "\\$1" );
}

// Clear an element and replace with a single child
function replace_child_with(id, new_child) {
    var elem = document.getElementById(id);
    // elem.replaceChild(new_child, elem.firstChild);
    delete_all_children(elem);
    elem.appendChild(new_child);
}

// Clear an element
function delete_all_children(elem) {
    while (elem.firstChild) {
        elem.firstChild.remove();
    }
}
