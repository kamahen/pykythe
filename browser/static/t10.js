// For the various cursors, see https://www.w3schools.com/cssref/playit.asp?filename=playcss_cursor

// global 'g_anchor_edges' gets {signature:str, edge:str, target:{corpus,root,path,language,signature} items
// (see t10_data.lines[line_key].edges)
var g_anchor_edges = [];

var t10_data;  // set by dynamic load
var t10_file_names;  // set by dynamic load

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

const token_css_color_class = {  // See t10.css
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

function set_src_txt() {
    // Note: this import requires CORS turned off (see README)
    // The "./" is not optional
    // t10_data.js is assumed to set t10_data
    import('./files/FILES.js').then(() => set_files_txt());
    import('./t10_data.js').then(() => set_src_txt_impl());
}

function set_files_txt() {
    var table = document.createElement('table');
    table.setAttribute('class', 'file_nav');
    var div = document.createElement('div');
    div_str = '';
    for (fn of t10_file_names) {
        // fn.path.split('/');
        var td1 = table.insertRow().insertCell();
        var txt_span = document.createElement('span');
        txt_span.onclick = function() { load_new_file('files/' + fn.filename); };
        txt_span.onmouseover = function(e) { e.currentTarget.classList.add('file_nav_hover'); }
        txt_span.onmouseleave = function(e) { e.currentTarget.classList.remove('file_nav_hover'); }
        txt_span.innerHTML = sanitize(fn.corpus) + ':' + sanitize(fn.root) + ':' + sanitize(fn.path);
        td1.appendChild(txt_span);
    }
    files_elem = document.getElementById('file_nav');
    for (ch of files_elem.childNodes) {
        files_elem.removeChild(ch);
    }
    files_elem.appendChild(table)
}

function load_new_file(filename) {
    alert(filename);
}

function set_src_txt_impl() {
    var table = document.createElement('table');
    table.setAttribute('class', 'src_table');
    if (t10_data.length != 1) {
        alert('Bad t10_data, length=' + t10_data.length);
        return;
    }
    for (const line_key of t10_data[0].line_keys) {
        const line_parts = t10_data[0].lines[line_key];
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
    src_elem = document.getElementById('src');
    for (ch of src_elem.childNodes) {
        src_elem.removeChild(ch);
    }
    src_elem.appendChild(table);
}

function do_for_signature(target, class_action, class_id) {
    const a_edges = anchor_target_edges(target.id);
    for (const a_edge of a_edges) {
        const t_a_edges = target_anchor_edges(a_edge.target);
        for (const t_a_edge of t_a_edges) {
            document.getElementById(t_a_edge.signature).classList[class_action](class_id);
        }
    }
}

function src_line_txt(parts, txt_span) {
    for (const part of parts) {
        var span = document.createElement('span');
        span.setAttribute('class', token_css_color_class[part.token_color]);
        span.innerHTML = sanitize(part.value);
        if (is_token_name[part.token_color]) {
            // console.log(part.token_color + ' ' + part.signature);
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
    // No need for .replace(/ /g, '&nbsp;') by using CSS white-space:pre
    return raw_str
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&apos;');
    }

function jq(id) {
    return '.' + id.replace( /(:|\.|\[|\]|,|=|@|<|>)/g, "\\$1" );
}
