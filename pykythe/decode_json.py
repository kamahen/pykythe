#!/usr/bin/python3.6
"""Read the base64-encoded Kythe JSON and decode it.

This assumes that the JSON is a single item per line and that the
fact_value items are UTF-8 when decoded.
"""

import base64
import json
import sys

for line in sys.stdin:
    as_json = json.loads(line)
    if 'fact_value' in as_json:
        as_json['fact_value'] = base64.b64decode(
            as_json['fact_value']).decode('ascii')
    print(json.dumps(as_json))
