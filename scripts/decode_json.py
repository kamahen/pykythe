#!/usr/bin/env python3.6
"""Read the base64-encoded Kythe JSON and decode it.

This assumes that the JSON is a single item per line and that the
fact_value items are UTF-8 when decoded.
"""

import base64
import json
import sys
import zlib

for line in sys.stdin:
    as_json = json.loads(line)
    if 'fact_value' in as_json:
        if as_json['fact_name'] == '/kythe/x-htmlgz':
            as_json['fact_value'] = zlib.decompress(
                base64.b64decode(as_json['fact_value'])).decode('utf-8')
        else:
            as_json['fact_value'] = base64.b64decode(
                as_json['fact_value']).decode('utf-8')
    print(json.dumps(as_json))
