#!/usr/bin/env python3
"""Fix missing trailing comma on httpBasicAuth ShapeID lines in smithy-generated models.py."""
import pathlib
import sys

p = pathlib.Path(sys.argv[1])
p.write_text(
    p.read_text().replace(
        '            ShapeID("smithy.api#httpBasicAuth")\n',
        '           ShapeID("smithy.api#httpBasicAuth"),\n',
    )
)
