// For the various cursors, see https://www.w3schools.com/cssref/playit.asp?filename=playcss_cursor
'use strict';

// global 'g_anchor_edges' gets {signature:str, edge:str, target:{corpus,root,path,language,signature} items
// (see color_data.lines[line_key].edges)
var g_anchor_edges = [];

var corpus_root_path_filename = null;  // set by dynamic load

function anchor_target_edges(anchor_signature) {
    return g_anchor_edges
        .filter(edge => edge.signature == anchor_signature);
}

function target_anchor_edges(target) {
    return g_anchor_edges
        .filter(edge =>
                edge.target.signature == target.signature &&
                edge.target.corpus == target.corpus &&
                edge.target.root == target.root &&
                edge.target.path == target.path &&
                edge.target.language == target.language);
}

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

function initialize() {
    fetch_from_server({fetch: 'FILES.js'}, set_files_nav_txt);
}

function load_new_file(corpus, root, path) {
    // Called from file browser onclick.
    const src_browser_file = find_file(corpus, root, path);
    if (!src_browser_file) {
        alert("Can't load " + path);
    } else {
        fetch_from_server({fetch: src_browser_file},
                          data => set_src_txt_impl(corpus, root, path, data));
    }
}

function set_files_nav_txt(corpus_root_path_filename_from_server) {
    // DO NOT SUBMIT -- shouldn't do the double parse
    corpus_root_path_filename = JSON.parse(corpus_root_path_filename_from_server.contents);
    var table = document.createElement('table');
    table.setAttribute('class', 'file_nav');
    var div = document.createElement('div');
    for (const fn of corpus_root_path_filename) {
        // fn.path.split('/');
        var td1 = table.insertRow().insertCell();
        var txt_span = document.createElement('span');
        txt_span.onclick = function(xfn) {
            return function() { load_new_file(xfn.corpus, xfn.root, xfn.path); }; }(fn);
        txt_span.onmouseover = function(e) { e.currentTarget.classList.add('file_nav_hover'); }
        txt_span.onmouseleave = function(e) { e.currentTarget.classList.remove('file_nav_hover'); }
        txt_span.innerHTML = sanitize(fn.corpus) + ':' + sanitize(fn.root) + ':' + sanitize(fn.path);
        td1.appendChild(txt_span);
    }
    replace_child_with('file_nav', table);  // 'file_nav'
}

function find_file(corpus, root, path) {
    for (const fn of corpus_root_path_filename) {
        if (fn.corpus == corpus &&
            fn.root == root &&
            fn.path == path) {
            return fn.filename;
        }
    }
    return null;
}

function set_src_txt_impl(corpus, root, path, color_data_str) {
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
            src_line_txt(line_parts, txt_span);
        } else {
            txt_span.innerHTML = '&nbsp;';
        }
        td2.appendChild(txt_span);
    }
    replace_child_with('src', table);
}

function do_for_signature(target, class_action, class_id) {
    for (const a_edge of anchor_target_edges(target.id)) {
        for (const t_a_edge of target_anchor_edges(a_edge.target)) {
            var edge = document.getElementById(t_a_edge.signature);
            if (edge) {
                edge.classList[class_action](class_id);
            } else {
                // TODO: this doesn't seem to happen consistently:
                console.log('No edge for ' + target.id + ' ' + t_a_edge.signature);
            }
        }
    }
}

function src_line_txt(parts, txt_span) {
    for (const part of parts) {
        var span = document.createElement('span');
        span.setAttribute('class', token_css_color_class[part.token_color]);
        span.innerHTML = sanitize(part.value);
        if (is_token_name[part.token_color]) {
            for (const p_edge of part.edges) {
                g_anchor_edges.push({signature: part.signature,
                                     edge: p_edge.edge,
                                     target: p_edge.target})
            }
            span.id = part.signature;
            span.onmouseover = function(e) { // e is MouseEvent
                do_for_signature(e.currentTarget, 'add', 'src_hover');
                return false;
            };
            span.onmouseleave = function(e) { // e is MouseEvent
                do_for_signature(e.currentTarget, 'remove', 'src_hover');
                return false;
            };
            span.onclick = function(e) { // e is MouseEvent
                return false;
            }
            span.oncontextmenu = function(e) { // e is MouseEvent
                return false;
            }
            txt_span.appendChild(span);
        } else {
            txt_span.appendChild(span);
        }
    }
}

function sanitize(raw_str) {
    // There shouldn't be a need for .replace(/ /g, '&nbsp;') if CSS
    // has white-space:pre ... but by experiment, it's needed.
    return (raw_str)
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&apos;')
        .replace(/ /g, '&nbsp;');
}

function fetch_from_server(request, callback) {
    // callback should take a single arg, the response from the server,
    // e.g.: json_data => set_files_nav_txt(json_data))
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

function replace_child_with(id, new_child) {
    var elem = document.getElementById(id);
    // elem.replaceChild(new_child, elem.firstChild);
    while (elem.firstChild) {
        elem.firstChild.remove();
    }
    elem.appendChild(new_child);
}
