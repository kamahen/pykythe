#!/usr/bin/env python3.7
"""Read the base64-encoded Kythe JSON and decode it.

This assumes that the JSON is a single item per line and that the
fact_value items are UTF-8 when decoded.
"""

import base64
import json
import sys
import zlib

for line in sys.stdin:
    line = line.strip()
    if not line:  # skip blank lines (typically at end of file)
        continue
    try:
        as_json = json.loads(line)
    except json.JSONDecodeError as exc:
        raise ValueError('Error %r - JSON line: %r' % (exc, line)) from exc
    if 'fact_value' in as_json:
        if as_json['fact_name'] == '/kythe/x-htmlgz':
            as_json['fact_value'] = zlib.decompress(base64.b64decode(
                    as_json['fact_value'])).decode('utf-8')
        elif as_json['fact_name'] == '/pykythe/symtab':
            pass  # It's unencoded
        else:
            # TODO: This doesn't work for iso-8859-1,
            #       e.g.: source for pytype/cpython/Tools/i18n/pygettext.py
            #       It would be best if this program had an argument with the
            #       file name, so that a nice message could be output.
            try:
                as_json['fact_value'] = base64.b64decode(as_json['fact_value']).decode('utf-8')
            except UnicodeDecodeError:
                try:
                    as_json['fact_value'] = base64.b64decode(
                            as_json['fact_value']).decode('iso-8859-1')
                except UnicodeDecodeError:
                    as_json['fact_value'] = base64.b64decode(as_json['fact_value']).decode(
                            'utf-8', 'surrogateescape')
    print(json.dumps(as_json))
