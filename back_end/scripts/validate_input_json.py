#!/usr/bin/env python3

# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

import json
import sys

from jsonschema import validate

json_path = sys.argv[1]

schema = {
    "$schema": "http://json-schema.org/draft-07/schema",
    "title": "DAnIEL CI schema",
    "type": "object",
    "required": ["version", "project_id"],
    "properties": {
        "version": {"type": "string", "pattern": "CI"},
        "start_datetime": {"type": "string"},
    },
}

with open(json_path, "r") as f:
    instance = json.load(f)
    validate(instance=instance, schema=schema)
