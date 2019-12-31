// output from
// cp /tmp/pykythe_test/KYTHE/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/t10.kythe.json /tmp/
// swipl -g get_and_print_color_text -t halt scripts/extract_color.pl </tmp/t10.kythe.json >/tmp/t10_data_raw.js
t10_data =
[
  {
    "corpus":"CORPUS",
    "language":"python",
    "line_keys": [
      "00000001",
      "00000002",
      "00000003",
      "00000004",
      "00000006",
      "00000008",
      "00000009",
      "00000011",
      "00000012",
      "00000014",
      "00000015",
      "00000017",
      "00000018",
      "00000019",
      "00000021",
      "00000023",
      "00000024",
      "00000025",
      "00000027",
      "00000028",
      "00000030",
      "00000031",
      "00000033",
      "00000034",
      "00000036",
      "00000037",
      "00000038",
      "00000039",
      "00000040",
      "00000042",
      "00000045",
      "00000046",
      "00000048",
      "00000049",
      "00000050",
      "00000051",
      "00000052",
      "00000055",
      "00000057",
      "00000058",
      "00000059",
      "00000060"
    ],
    "lines": {
      "00000001": [],
      "00000002": [
	{
	  "column":0,
	  "edges": [],
	  "end":4,
	  "lineno":2,
	  "signature":"@1:4<def>",
	  "start":1,
	  "token_color":"<KEYWORD>",
	  "value":"def"
	},
	{
	  "column":3,
	  "edges": [],
	  "end":5,
	  "lineno":2,
	  "signature":"",
	  "start":4,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":4,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.foo"
	      }
	    }
	  ],
	  "end":8,
	  "lineno":2,
	  "signature":"@5:8<foo>",
	  "start":5,
	  "token_color":"<VAR_BINDING>",
	  "value":"foo"
	},
	{
	  "column":7,
	  "edges": [],
	  "end":9,
	  "lineno":2,
	  "signature":"",
	  "start":8,
	  "token_color":"<PUNCTUATION>",
	  "value":"("
	},
	{
	  "column":8,
	  "edges": [],
	  "end":10,
	  "lineno":2,
	  "signature":"",
	  "start":9,
	  "token_color":"<PUNCTUATION>",
	  "value":")"
	},
	{
	  "column":9,
	  "edges": [],
	  "end":11,
	  "lineno":2,
	  "signature":"",
	  "start":10,
	  "token_color":"<PUNCTUATION>",
	  "value":":"
	}
      ],
      "00000003": [
	{
	  "column":0,
	  "edges": [],
	  "end":16,
	  "lineno":3,
	  "signature":"",
	  "start":12,
	  "token_color":"<WHITESPACE>",
	  "value":"    "
	},
	{
	  "column":4,
	  "edges": [],
	  "end":37,
	  "lineno":3,
	  "signature":"",
	  "start":16,
	  "token_color":"<COMMENT>",
	  "value":"# 0x251c (''\\u251c'')"
	}
      ],
      "00000004": [
	{
	  "column":0,
	  "edges": [],
	  "end":42,
	  "lineno":4,
	  "signature":"",
	  "start":38,
	  "token_color":"<WHITESPACE>",
	  "value":"    "
	},
	{
	  "column":4,
	  "edges": [],
	  "end":48,
	  "lineno":4,
	  "signature":"@42:48<return>",
	  "start":42,
	  "token_color":"<KEYWORD>",
	  "value":"return"
	},
	{
	  "column":10,
	  "edges": [],
	  "end":49,
	  "lineno":4,
	  "signature":"",
	  "start":48,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":11,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".tmp.pykythe_test.SUBST.BUILTINS.builtins.format"
	      }
	    }
	  ],
	  "end":55,
	  "lineno":4,
	  "signature":"@49:55<format>",
	  "start":49,
	  "token_color":"<VAR_REF>",
	  "value":"format"
	},
	{
	  "column":17,
	  "edges": [],
	  "end":56,
	  "lineno":4,
	  "signature":"",
	  "start":55,
	  "token_color":"<PUNCTUATION>",
	  "value":"("
	},
	{
	  "column":18,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.qqsv"
	      }
	    }
	  ],
	  "end":60,
	  "lineno":4,
	  "signature":"@56:60<qqsv>",
	  "start":56,
	  "token_color":"<VAR_REF>",
	  "value":"qqsv"
	},
	{
	  "column":22,
	  "edges": [],
	  "end":61,
	  "lineno":4,
	  "signature":"",
	  "start":60,
	  "token_color":"<PUNCTUATION>",
	  "value":"("
	},
	{
	  "column":23,
	  "edges": [],
	  "end":64,
	  "lineno":4,
	  "signature":"",
	  "start":61,
	  "token_color":"<STRING>",
	  "value":"'├'"
	},
	{
	  "column":26,
	  "edges": [],
	  "end":67,
	  "lineno":4,
	  "signature":"",
	  "start":66,
	  "token_color":"<PUNCTUATION>",
	  "value":")"
	},
	{
	  "column":27,
	  "edges": [],
	  "end":68,
	  "lineno":4,
	  "signature":"",
	  "start":67,
	  "token_color":"<PUNCTUATION>",
	  "value":","
	},
	{
	  "column":30,
	  "edges": [],
	  "end":69,
	  "lineno":4,
	  "signature":"",
	  "start":68,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":29,
	  "edges": [],
	  "end":74,
	  "lineno":4,
	  "signature":"",
	  "start":69,
	  "token_color":"<STRING>",
	  "value":"'02x'"
	},
	{
	  "column":34,
	  "edges": [],
	  "end":75,
	  "lineno":4,
	  "signature":"",
	  "start":74,
	  "token_color":"<PUNCTUATION>",
	  "value":")"
	}
      ],
      "00000006": [
	{
	  "column":4,
	  "edges": [],
	  "end":107,
	  "lineno":6,
	  "signature":"",
	  "start":79,
	  "token_color":"<COMMENT>",
	  "value":"# xformat(xqqsv('├'), '02x')"
	}
      ],
      "00000008": [
	{
	  "column":4,
	  "edges": [],
	  "end":180,
	  "lineno":8,
	  "signature":"",
	  "start":109,
	  "token_color":"<COMMENT>",
	  "value":"#- { @str ref vname(\"${BUILTINS_FQN}.builtins.str\", _, _, \"\", python) }"
	}
      ],
      "00000009": [
	{
	  "column":0,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.aa"
	      }
	    }
	  ],
	  "end":183,
	  "lineno":9,
	  "signature":"@181:183<aa>",
	  "start":181,
	  "token_color":"<VAR_BINDING>",
	  "value":"aa"
	},
	{
	  "column":6,
	  "edges": [],
	  "end":184,
	  "lineno":9,
	  "signature":"",
	  "start":183,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":3,
	  "edges": [],
	  "end":185,
	  "lineno":9,
	  "signature":"",
	  "start":184,
	  "token_color":"<PUNCTUATION>",
	  "value":"="
	},
	{
	  "column":8,
	  "edges": [],
	  "end":186,
	  "lineno":9,
	  "signature":"",
	  "start":185,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":5,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".tmp.pykythe_test.SUBST.BUILTINS.builtins.str"
	      }
	    }
	  ],
	  "end":189,
	  "lineno":9,
	  "signature":"@186:189<str>",
	  "start":186,
	  "token_color":"<VAR_REF>",
	  "value":"str"
	}
      ],
      "00000011": [
	{
	  "column":4,
	  "edges": [],
	  "end":262,
	  "lineno":11,
	  "signature":"",
	  "start":191,
	  "token_color":"<COMMENT>",
	  "value":"#- { @str ref vname(\"${BUILTINS_FQN}.builtins.str\", _, _, \"\", python) }"
	}
      ],
      "00000012": [
	{
	  "column":0,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.bb"
	      }
	    }
	  ],
	  "end":265,
	  "lineno":12,
	  "signature":"@263:265<bb>",
	  "start":263,
	  "token_color":"<VAR_BINDING>",
	  "value":"bb"
	},
	{
	  "column":6,
	  "edges": [],
	  "end":266,
	  "lineno":12,
	  "signature":"",
	  "start":265,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":3,
	  "edges": [],
	  "end":267,
	  "lineno":12,
	  "signature":"",
	  "start":266,
	  "token_color":"<PUNCTUATION>",
	  "value":"="
	},
	{
	  "column":8,
	  "edges": [],
	  "end":268,
	  "lineno":12,
	  "signature":"",
	  "start":267,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":5,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".tmp.pykythe_test.SUBST.BUILTINS.builtins.str"
	      }
	    }
	  ],
	  "end":271,
	  "lineno":12,
	  "signature":"@268:271<str>",
	  "start":268,
	  "token_color":"<VAR_REF>",
	  "value":"str"
	},
	{
	  "column":8,
	  "edges": [],
	  "end":272,
	  "lineno":12,
	  "signature":"",
	  "start":271,
	  "token_color":"<PUNCTUATION>",
	  "value":"("
	},
	{
	  "column":9,
	  "edges": [],
	  "end":273,
	  "lineno":12,
	  "signature":"",
	  "start":272,
	  "token_color":"<PUNCTUATION>",
	  "value":")"
	}
      ],
      "00000014": [
	{
	  "column":4,
	  "edges": [],
	  "end":346,
	  "lineno":14,
	  "signature":"",
	  "start":275,
	  "token_color":"<COMMENT>",
	  "value":"#- { @str ref vname(\"${BUILTINS_FQN}.builtins.str\", _, _, \"\", python) }"
	}
      ],
      "00000015": [
	{
	  "column":0,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.ee"
	      }
	    }
	  ],
	  "end":349,
	  "lineno":15,
	  "signature":"@347:349<ee>",
	  "start":347,
	  "token_color":"<VAR_BINDING>",
	  "value":"ee"
	},
	{
	  "column":2,
	  "edges": [],
	  "end":350,
	  "lineno":15,
	  "signature":"",
	  "start":349,
	  "token_color":"<PUNCTUATION>",
	  "value":":"
	},
	{
	  "column":7,
	  "edges": [],
	  "end":351,
	  "lineno":15,
	  "signature":"",
	  "start":350,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":4,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".tmp.pykythe_test.SUBST.BUILTINS.builtins.str"
	      }
	    }
	  ],
	  "end":354,
	  "lineno":15,
	  "signature":"@351:354<str>",
	  "start":351,
	  "token_color":"<VAR_REF>",
	  "value":"str"
	},
	{
	  "column":11,
	  "edges": [],
	  "end":355,
	  "lineno":15,
	  "signature":"",
	  "start":354,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":12,
	  "edges": [],
	  "end":364,
	  "lineno":15,
	  "signature":"",
	  "start":355,
	  "token_color":"<COMMENT>",
	  "value":"# = 'abc'"
	}
      ],
      "00000017": [
	{
	  "column":0,
	  "edges": [],
	  "end":369,
	  "lineno":17,
	  "signature":"@366:369<def>",
	  "start":366,
	  "token_color":"<KEYWORD>",
	  "value":"def"
	},
	{
	  "column":7,
	  "edges": [],
	  "end":370,
	  "lineno":17,
	  "signature":"",
	  "start":369,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":4,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.foo"
	      }
	    }
	  ],
	  "end":373,
	  "lineno":17,
	  "signature":"@370:373<foo>",
	  "start":370,
	  "token_color":"<VAR_BINDING>",
	  "value":"foo"
	},
	{
	  "column":7,
	  "edges": [],
	  "end":374,
	  "lineno":17,
	  "signature":"",
	  "start":373,
	  "token_color":"<PUNCTUATION>",
	  "value":"("
	},
	{
	  "column":8,
	  "edges": [],
	  "end":375,
	  "lineno":17,
	  "signature":"",
	  "start":374,
	  "token_color":"<PUNCTUATION>",
	  "value":")"
	},
	{
	  "column":9,
	  "edges": [],
	  "end":376,
	  "lineno":17,
	  "signature":"",
	  "start":375,
	  "token_color":"<PUNCTUATION>",
	  "value":":"
	}
      ],
      "00000018": [
	{
	  "column":4,
	  "edges": [],
	  "end":381,
	  "lineno":18,
	  "signature":"",
	  "start":377,
	  "token_color":"<WHITESPACE>",
	  "value":"    "
	},
	{
	  "column":8,
	  "edges": [],
	  "end":452,
	  "lineno":18,
	  "signature":"",
	  "start":381,
	  "token_color":"<COMMENT>",
	  "value":"#- { @str ref vname(\"${BUILTINS_FQN}.builtins.str\", _, _, \"\", python) }"
	}
      ],
      "00000019": [
	{
	  "column":4,
	  "edges": [],
	  "end":457,
	  "lineno":19,
	  "signature":"",
	  "start":453,
	  "token_color":"<WHITESPACE>",
	  "value":"    "
	},
	{
	  "column":4,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.foo.<local>.ff"
	      }
	    }
	  ],
	  "end":459,
	  "lineno":19,
	  "signature":"@457:459<ff>",
	  "start":457,
	  "token_color":"<VAR_BINDING>",
	  "value":"ff"
	},
	{
	  "column":6,
	  "edges": [],
	  "end":460,
	  "lineno":19,
	  "signature":"",
	  "start":459,
	  "token_color":"<PUNCTUATION>",
	  "value":":"
	},
	{
	  "column":11,
	  "edges": [],
	  "end":461,
	  "lineno":19,
	  "signature":"",
	  "start":460,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":8,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".tmp.pykythe_test.SUBST.BUILTINS.builtins.str"
	      }
	    }
	  ],
	  "end":464,
	  "lineno":19,
	  "signature":"@461:464<str>",
	  "start":461,
	  "token_color":"<VAR_REF>",
	  "value":"str"
	},
	{
	  "column":15,
	  "edges": [],
	  "end":465,
	  "lineno":19,
	  "signature":"",
	  "start":464,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":12,
	  "edges": [],
	  "end":466,
	  "lineno":19,
	  "signature":"",
	  "start":465,
	  "token_color":"<PUNCTUATION>",
	  "value":"="
	},
	{
	  "column":17,
	  "edges": [],
	  "end":467,
	  "lineno":19,
	  "signature":"",
	  "start":466,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":14,
	  "edges": [],
	  "end":472,
	  "lineno":19,
	  "signature":"",
	  "start":467,
	  "token_color":"<STRING>",
	  "value":"'xyz'"
	}
      ],
      "00000021": [
	{
	  "column":0,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.foo"
	      }
	    }
	  ],
	  "end":477,
	  "lineno":21,
	  "signature":"@474:477<foo>",
	  "start":474,
	  "token_color":"<VAR_REF>",
	  "value":"foo"
	},
	{
	  "column":3,
	  "edges": [],
	  "end":478,
	  "lineno":21,
	  "signature":"",
	  "start":477,
	  "token_color":"<PUNCTUATION>",
	  "value":"("
	},
	{
	  "column":4,
	  "edges": [],
	  "end":479,
	  "lineno":21,
	  "signature":"",
	  "start":478,
	  "token_color":"<PUNCTUATION>",
	  "value":")"
	}
      ],
      "00000023": [
	{
	  "column":0,
	  "edges": [],
	  "end":486,
	  "lineno":23,
	  "signature":"@481:486<class>",
	  "start":481,
	  "token_color":"<KEYWORD>",
	  "value":"class"
	},
	{
	  "column":6,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C"
	      }
	    }
	  ],
	  "end":488,
	  "lineno":23,
	  "signature":"@487:488<C>",
	  "start":487,
	  "token_color":"<VAR_BINDING>",
	  "value":"C"
	},
	{
	  "column":7,
	  "edges": [],
	  "end":489,
	  "lineno":23,
	  "signature":"",
	  "start":488,
	  "token_color":"<PUNCTUATION>",
	  "value":":"
	}
      ],
      "00000024": [
	{
	  "column":0,
	  "edges": [],
	  "end":487,
	  "lineno":24,
	  "signature":"",
	  "start":486,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":4,
	  "edges": [],
	  "end":494,
	  "lineno":24,
	  "signature":"",
	  "start":490,
	  "token_color":"<WHITESPACE>",
	  "value":"    "
	},
	{
	  "column":4,
	  "edges": [],
	  "end":497,
	  "lineno":24,
	  "signature":"@494:497<def>",
	  "start":494,
	  "token_color":"<KEYWORD>",
	  "value":"def"
	},
	{
	  "column":11,
	  "edges": [],
	  "end":498,
	  "lineno":24,
	  "signature":"",
	  "start":497,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":8,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C.__init__"
	      }
	    }
	  ],
	  "end":506,
	  "lineno":24,
	  "signature":"@498:506<__init__>",
	  "start":498,
	  "token_color":"<VAR_BINDING>",
	  "value":"__init__"
	},
	{
	  "column":16,
	  "edges": [],
	  "end":507,
	  "lineno":24,
	  "signature":"",
	  "start":506,
	  "token_color":"<PUNCTUATION>",
	  "value":"("
	},
	{
	  "column":17,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C.__init__.<local>.self"
	      }
	    }
	  ],
	  "end":511,
	  "lineno":24,
	  "signature":"@507:511<self>",
	  "start":507,
	  "token_color":"<VAR_BINDING>",
	  "value":"self"
	},
	{
	  "column":21,
	  "edges": [],
	  "end":512,
	  "lineno":24,
	  "signature":"",
	  "start":511,
	  "token_color":"<PUNCTUATION>",
	  "value":")"
	},
	{
	  "column":22,
	  "edges": [],
	  "end":513,
	  "lineno":24,
	  "signature":"",
	  "start":512,
	  "token_color":"<PUNCTUATION>",
	  "value":":"
	}
      ],
      "00000025": [
	{
	  "column":4,
	  "edges": [],
	  "end":522,
	  "lineno":25,
	  "signature":"",
	  "start":514,
	  "token_color":"<WHITESPACE>",
	  "value":"        "
	},
	{
	  "column":8,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C.__init__.<local>.self"
	      }
	    }
	  ],
	  "end":526,
	  "lineno":25,
	  "signature":"@522:526<self>",
	  "start":522,
	  "token_color":"<VAR_REF>",
	  "value":"self"
	},
	{
	  "column":12,
	  "edges": [],
	  "end":527,
	  "lineno":25,
	  "signature":"",
	  "start":526,
	  "token_color":"<PUNCTUATION>",
	  "value":"."
	},
	{
	  "column":13,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C.x"
	      }
	    }
	  ],
	  "end":528,
	  "lineno":25,
	  "signature":"@527:528<x>",
	  "start":527,
	  "token_color":"<ATTR_BINDING>",
	  "value":"x"
	},
	{
	  "column":18,
	  "edges": [],
	  "end":529,
	  "lineno":25,
	  "signature":"",
	  "start":528,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":15,
	  "edges": [],
	  "end":530,
	  "lineno":25,
	  "signature":"",
	  "start":529,
	  "token_color":"<PUNCTUATION>",
	  "value":"="
	},
	{
	  "column":20,
	  "edges": [],
	  "end":531,
	  "lineno":25,
	  "signature":"",
	  "start":530,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":17,
	  "edges": [],
	  "end":534,
	  "lineno":25,
	  "signature":"",
	  "start":531,
	  "token_color":"<NUMBER>",
	  "value":"123"
	}
      ],
      "00000027": [
	{
	  "column":4,
	  "edges": [],
	  "end":540,
	  "lineno":27,
	  "signature":"",
	  "start":536,
	  "token_color":"<WHITESPACE>",
	  "value":"    "
	},
	{
	  "column":4,
	  "edges": [],
	  "end":543,
	  "lineno":27,
	  "signature":"@540:543<def>",
	  "start":540,
	  "token_color":"<KEYWORD>",
	  "value":"def"
	},
	{
	  "column":11,
	  "edges": [],
	  "end":544,
	  "lineno":27,
	  "signature":"",
	  "start":543,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":8,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C.set_x"
	      }
	    }
	  ],
	  "end":549,
	  "lineno":27,
	  "signature":"@544:549<set_x>",
	  "start":544,
	  "token_color":"<VAR_BINDING>",
	  "value":"set_x"
	},
	{
	  "column":13,
	  "edges": [],
	  "end":550,
	  "lineno":27,
	  "signature":"",
	  "start":549,
	  "token_color":"<PUNCTUATION>",
	  "value":"("
	},
	{
	  "column":14,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C.set_x.<local>.self"
	      }
	    }
	  ],
	  "end":554,
	  "lineno":27,
	  "signature":"@550:554<self>",
	  "start":550,
	  "token_color":"<VAR_BINDING>",
	  "value":"self"
	},
	{
	  "column":18,
	  "edges": [],
	  "end":555,
	  "lineno":27,
	  "signature":"",
	  "start":554,
	  "token_color":"<PUNCTUATION>",
	  "value":","
	},
	{
	  "column":23,
	  "edges": [],
	  "end":556,
	  "lineno":27,
	  "signature":"",
	  "start":555,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":20,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C.set_x.<local>.x"
	      }
	    }
	  ],
	  "end":557,
	  "lineno":27,
	  "signature":"@556:557<x>",
	  "start":556,
	  "token_color":"<VAR_BINDING>",
	  "value":"x"
	},
	{
	  "column":21,
	  "edges": [],
	  "end":558,
	  "lineno":27,
	  "signature":"",
	  "start":557,
	  "token_color":"<PUNCTUATION>",
	  "value":")"
	},
	{
	  "column":22,
	  "edges": [],
	  "end":559,
	  "lineno":27,
	  "signature":"",
	  "start":558,
	  "token_color":"<PUNCTUATION>",
	  "value":":"
	}
      ],
      "00000028": [
	{
	  "column":4,
	  "edges": [],
	  "end":568,
	  "lineno":28,
	  "signature":"",
	  "start":560,
	  "token_color":"<WHITESPACE>",
	  "value":"        "
	},
	{
	  "column":8,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C.set_x.<local>.self"
	      }
	    }
	  ],
	  "end":572,
	  "lineno":28,
	  "signature":"@568:572<self>",
	  "start":568,
	  "token_color":"<VAR_REF>",
	  "value":"self"
	},
	{
	  "column":12,
	  "edges": [],
	  "end":573,
	  "lineno":28,
	  "signature":"",
	  "start":572,
	  "token_color":"<PUNCTUATION>",
	  "value":"."
	},
	{
	  "column":13,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C.x"
	      }
	    }
	  ],
	  "end":574,
	  "lineno":28,
	  "signature":"@573:574<x>",
	  "start":573,
	  "token_color":"<ATTR_BINDING>",
	  "value":"x"
	},
	{
	  "column":18,
	  "edges": [],
	  "end":575,
	  "lineno":28,
	  "signature":"",
	  "start":574,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":15,
	  "edges": [],
	  "end":576,
	  "lineno":28,
	  "signature":"",
	  "start":575,
	  "token_color":"<PUNCTUATION>",
	  "value":"="
	},
	{
	  "column":17,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C.set_x.<local>.x"
	      }
	    }
	  ],
	  "end":578,
	  "lineno":28,
	  "signature":"@577:578<x>",
	  "start":577,
	  "token_color":"<VAR_REF>",
	  "value":"x"
	}
      ],
      "00000030": [
	{
	  "column":0,
	  "edges": [],
	  "end":577,
	  "lineno":30,
	  "signature":"",
	  "start":576,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":0,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.c"
	      }
	    }
	  ],
	  "end":581,
	  "lineno":30,
	  "signature":"@580:581<c>",
	  "start":580,
	  "token_color":"<VAR_BINDING>",
	  "value":"c"
	},
	{
	  "column":5,
	  "edges": [],
	  "end":582,
	  "lineno":30,
	  "signature":"",
	  "start":581,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":2,
	  "edges": [],
	  "end":583,
	  "lineno":30,
	  "signature":"",
	  "start":582,
	  "token_color":"<PUNCTUATION>",
	  "value":"="
	},
	{
	  "column":7,
	  "edges": [],
	  "end":584,
	  "lineno":30,
	  "signature":"",
	  "start":583,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":4,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C"
	      }
	    }
	  ],
	  "end":585,
	  "lineno":30,
	  "signature":"@584:585<C>",
	  "start":584,
	  "token_color":"<VAR_REF>",
	  "value":"C"
	},
	{
	  "column":5,
	  "edges": [],
	  "end":586,
	  "lineno":30,
	  "signature":"",
	  "start":585,
	  "token_color":"<PUNCTUATION>",
	  "value":"("
	},
	{
	  "column":6,
	  "edges": [],
	  "end":587,
	  "lineno":30,
	  "signature":"",
	  "start":586,
	  "token_color":"<PUNCTUATION>",
	  "value":")"
	}
      ],
      "00000031": [
	{
	  "column":0,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".tmp.pykythe_test.SUBST.BUILTINS.builtins.print"
	      }
	    }
	  ],
	  "end":593,
	  "lineno":31,
	  "signature":"@588:593<print>",
	  "start":588,
	  "token_color":"<VAR_REF>",
	  "value":"print"
	},
	{
	  "column":5,
	  "edges": [],
	  "end":594,
	  "lineno":31,
	  "signature":"",
	  "start":593,
	  "token_color":"<PUNCTUATION>",
	  "value":"("
	},
	{
	  "column":6,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.c"
	      }
	    }
	  ],
	  "end":595,
	  "lineno":31,
	  "signature":"@594:595<c>",
	  "start":594,
	  "token_color":"<VAR_REF>",
	  "value":"c"
	},
	{
	  "column":7,
	  "edges": [],
	  "end":596,
	  "lineno":31,
	  "signature":"",
	  "start":595,
	  "token_color":"<PUNCTUATION>",
	  "value":"."
	},
	{
	  "column":8,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C.x"
	      }
	    }
	  ],
	  "end":597,
	  "lineno":31,
	  "signature":"@596:597<x>",
	  "start":596,
	  "token_color":"<ATTR_REF>",
	  "value":"x"
	},
	{
	  "column":9,
	  "edges": [],
	  "end":598,
	  "lineno":31,
	  "signature":"",
	  "start":597,
	  "token_color":"<PUNCTUATION>",
	  "value":")"
	}
      ],
      "00000033": [
	{
	  "column":0,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.c2"
	      }
	    }
	  ],
	  "end":602,
	  "lineno":33,
	  "signature":"@600:602<c2>",
	  "start":600,
	  "token_color":"<VAR_BINDING>",
	  "value":"c2"
	},
	{
	  "column":6,
	  "edges": [],
	  "end":603,
	  "lineno":33,
	  "signature":"",
	  "start":602,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":3,
	  "edges": [],
	  "end":604,
	  "lineno":33,
	  "signature":"",
	  "start":603,
	  "token_color":"<PUNCTUATION>",
	  "value":"="
	},
	{
	  "column":8,
	  "edges": [],
	  "end":605,
	  "lineno":33,
	  "signature":"",
	  "start":604,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":5,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C"
	      }
	    }
	  ],
	  "end":606,
	  "lineno":33,
	  "signature":"@605:606<C>",
	  "start":605,
	  "token_color":"<VAR_REF>",
	  "value":"C"
	},
	{
	  "column":6,
	  "edges": [],
	  "end":607,
	  "lineno":33,
	  "signature":"",
	  "start":606,
	  "token_color":"<PUNCTUATION>",
	  "value":"("
	},
	{
	  "column":7,
	  "edges": [],
	  "end":608,
	  "lineno":33,
	  "signature":"",
	  "start":607,
	  "token_color":"<PUNCTUATION>",
	  "value":")"
	}
      ],
      "00000034": [
	{
	  "column":0,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.c2"
	      }
	    }
	  ],
	  "end":611,
	  "lineno":34,
	  "signature":"@609:611<c2>",
	  "start":609,
	  "token_color":"<VAR_REF>",
	  "value":"c2"
	},
	{
	  "column":2,
	  "edges": [],
	  "end":612,
	  "lineno":34,
	  "signature":"",
	  "start":611,
	  "token_color":"<PUNCTUATION>",
	  "value":"."
	},
	{
	  "column":3,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.C.x"
	      }
	    }
	  ],
	  "end":613,
	  "lineno":34,
	  "signature":"@612:613<x>",
	  "start":612,
	  "token_color":"<ATTR_BINDING>",
	  "value":"x"
	},
	{
	  "column":8,
	  "edges": [],
	  "end":614,
	  "lineno":34,
	  "signature":"",
	  "start":613,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":5,
	  "edges": [],
	  "end":615,
	  "lineno":34,
	  "signature":"",
	  "start":614,
	  "token_color":"<PUNCTUATION>",
	  "value":"="
	},
	{
	  "column":10,
	  "edges": [],
	  "end":616,
	  "lineno":34,
	  "signature":"",
	  "start":615,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":7,
	  "edges": [],
	  "end":619,
	  "lineno":34,
	  "signature":"",
	  "start":616,
	  "token_color":"<NUMBER>",
	  "value":"156"
	}
      ],
      "00000036": [
	{
	  "column":4,
	  "edges": [],
	  "end":641,
	  "lineno":36,
	  "signature":"",
	  "start":621,
	  "token_color":"<COMMENT>",
	  "value":"#- { @pow ref POW? }"
	}
      ],
      "00000037": [
	{
	  "column":4,
	  "edges": [],
	  "end":794,
	  "lineno":37,
	  "signature":"",
	  "start":642,
	  "token_color":"<COMMENT>",
	  "value":"#- // { POW./pykythe/type POW_type? } // TODO: \".tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.t10.pow\"./pykythe/type is set ... should it be?"
	}
      ],
      "00000038": [
	{
	  "column":0,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".tmp.pykythe_test.SUBST.BUILTINS.builtins.pow"
	      }
	    }
	  ],
	  "end":798,
	  "lineno":38,
	  "signature":"@795:798<pow>",
	  "start":795,
	  "token_color":"<VAR_REF>",
	  "value":"pow"
	},
	{
	  "column":3,
	  "edges": [],
	  "end":799,
	  "lineno":38,
	  "signature":"",
	  "start":798,
	  "token_color":"<PUNCTUATION>",
	  "value":"("
	},
	{
	  "column":4,
	  "edges": [],
	  "end":800,
	  "lineno":38,
	  "signature":"",
	  "start":799,
	  "token_color":"<NUMBER>",
	  "value":"1"
	},
	{
	  "column":5,
	  "edges": [],
	  "end":801,
	  "lineno":38,
	  "signature":"",
	  "start":800,
	  "token_color":"<PUNCTUATION>",
	  "value":","
	},
	{
	  "column":7,
	  "edges": [],
	  "end":803,
	  "lineno":38,
	  "signature":"",
	  "start":802,
	  "token_color":"<NUMBER>",
	  "value":"2"
	},
	{
	  "column":8,
	  "edges": [],
	  "end":804,
	  "lineno":38,
	  "signature":"",
	  "start":803,
	  "token_color":"<PUNCTUATION>",
	  "value":")"
	}
      ],
      "00000039": [
	{
	  "column":0,
	  "edges": [],
	  "end":802,
	  "lineno":39,
	  "signature":"",
	  "start":801,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	}
      ],
      "00000040": [
	{
	  "column":0,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.a"
	      }
	    }
	  ],
	  "end":807,
	  "lineno":40,
	  "signature":"@806:807<a>",
	  "start":806,
	  "token_color":"<VAR_BINDING>",
	  "value":"a"
	},
	{
	  "column":5,
	  "edges": [],
	  "end":808,
	  "lineno":40,
	  "signature":"",
	  "start":807,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":2,
	  "edges": [],
	  "end":809,
	  "lineno":40,
	  "signature":"",
	  "start":808,
	  "token_color":"<PUNCTUATION>",
	  "value":"="
	},
	{
	  "column":7,
	  "edges": [],
	  "end":810,
	  "lineno":40,
	  "signature":"",
	  "start":809,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":4,
	  "edges": [],
	  "end":813,
	  "lineno":40,
	  "signature":"",
	  "start":810,
	  "token_color":"<STRING>",
	  "value":"'a'"
	},
	{
	  "column":11,
	  "edges": [],
	  "end":814,
	  "lineno":40,
	  "signature":"",
	  "start":813,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":8,
	  "edges": [],
	  "end":816,
	  "lineno":40,
	  "signature":"@814:816<if>",
	  "start":814,
	  "token_color":"<KEYWORD>",
	  "value":"if"
	},
	{
	  "column":14,
	  "edges": [],
	  "end":817,
	  "lineno":40,
	  "signature":"",
	  "start":816,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":11,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.ee"
	      }
	    }
	  ],
	  "end":819,
	  "lineno":40,
	  "signature":"@817:819<ee>",
	  "start":817,
	  "token_color":"<VAR_REF>",
	  "value":"ee"
	},
	{
	  "column":17,
	  "edges": [],
	  "end":820,
	  "lineno":40,
	  "signature":"",
	  "start":819,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":14,
	  "edges": [],
	  "end":824,
	  "lineno":40,
	  "signature":"@820:824<else>",
	  "start":820,
	  "token_color":"<KEYWORD>",
	  "value":"else"
	},
	{
	  "column":22,
	  "edges": [],
	  "end":825,
	  "lineno":40,
	  "signature":"",
	  "start":824,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":19,
	  "edges": [],
	  "end":828,
	  "lineno":40,
	  "signature":"",
	  "start":825,
	  "token_color":"<STRING>",
	  "value":"'b'"
	}
      ],
      "00000042": [
	{
	  "column":0,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.b"
	      }
	    }
	  ],
	  "end":831,
	  "lineno":42,
	  "signature":"@830:831<b>",
	  "start":830,
	  "token_color":"<VAR_BINDING>",
	  "value":"b"
	},
	{
	  "column":5,
	  "edges": [],
	  "end":832,
	  "lineno":42,
	  "signature":"",
	  "start":831,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":2,
	  "edges": [],
	  "end":833,
	  "lineno":42,
	  "signature":"",
	  "start":832,
	  "token_color":"<PUNCTUATION>",
	  "value":"="
	},
	{
	  "column":7,
	  "edges": [],
	  "end":834,
	  "lineno":42,
	  "signature":"",
	  "start":833,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":4,
	  "edges": [],
	  "end":835,
	  "lineno":42,
	  "signature":"",
	  "start":834,
	  "token_color":"<PUNCTUATION>",
	  "value":"["
	},
	{
	  "column":5,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.<comp_for>[837,840].x"
	      }
	    }
	  ],
	  "end":836,
	  "lineno":42,
	  "signature":"@835:836<x>",
	  "start":835,
	  "token_color":"<VAR_REF>",
	  "value":"x"
	},
	{
	  "column":10,
	  "edges": [],
	  "end":837,
	  "lineno":42,
	  "signature":"",
	  "start":836,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":7,
	  "edges": [],
	  "end":840,
	  "lineno":42,
	  "signature":"@837:840<for>",
	  "start":837,
	  "token_color":"<KEYWORD>",
	  "value":"for"
	},
	{
	  "column":14,
	  "edges": [],
	  "end":841,
	  "lineno":42,
	  "signature":"",
	  "start":840,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":11,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.<comp_for>[837,840].x"
	      }
	    }
	  ],
	  "end":842,
	  "lineno":42,
	  "signature":"@841:842<x>",
	  "start":841,
	  "token_color":"<VAR_BINDING>",
	  "value":"x"
	},
	{
	  "column":16,
	  "edges": [],
	  "end":843,
	  "lineno":42,
	  "signature":"",
	  "start":842,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":13,
	  "edges": [],
	  "end":845,
	  "lineno":42,
	  "signature":"@843:845<in>",
	  "start":843,
	  "token_color":"<KEYWORD>",
	  "value":"in"
	},
	{
	  "column":19,
	  "edges": [],
	  "end":846,
	  "lineno":42,
	  "signature":"",
	  "start":845,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":16,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.ee"
	      }
	    }
	  ],
	  "end":848,
	  "lineno":42,
	  "signature":"@846:848<ee>",
	  "start":846,
	  "token_color":"<VAR_REF>",
	  "value":"ee"
	},
	{
	  "column":22,
	  "edges": [],
	  "end":849,
	  "lineno":42,
	  "signature":"",
	  "start":848,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":19,
	  "edges": [],
	  "end":851,
	  "lineno":42,
	  "signature":"@849:851<if>",
	  "start":849,
	  "token_color":"<KEYWORD>",
	  "value":"if"
	},
	{
	  "column":25,
	  "edges": [],
	  "end":852,
	  "lineno":42,
	  "signature":"",
	  "start":851,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":22,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.<comp_for>[837,840].x"
	      }
	    }
	  ],
	  "end":853,
	  "lineno":42,
	  "signature":"@852:853<x>",
	  "start":852,
	  "token_color":"<VAR_REF>",
	  "value":"x"
	},
	{
	  "column":23,
	  "edges": [],
	  "end":854,
	  "lineno":42,
	  "signature":"",
	  "start":853,
	  "token_color":"<PUNCTUATION>",
	  "value":"."
	},
	{
	  "column":24,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".tmp.pykythe_test.SUBST.BUILTINS.builtins.str.isprintable"
	      }
	    }
	  ],
	  "end":865,
	  "lineno":42,
	  "signature":"@854:865<isprintable>",
	  "start":854,
	  "token_color":"<ATTR_REF>",
	  "value":"isprintable"
	},
	{
	  "column":35,
	  "edges": [],
	  "end":866,
	  "lineno":42,
	  "signature":"",
	  "start":865,
	  "token_color":"<PUNCTUATION>",
	  "value":"("
	},
	{
	  "column":36,
	  "edges": [],
	  "end":867,
	  "lineno":42,
	  "signature":"",
	  "start":866,
	  "token_color":"<PUNCTUATION>",
	  "value":")"
	},
	{
	  "column":37,
	  "edges": [],
	  "end":868,
	  "lineno":42,
	  "signature":"",
	  "start":867,
	  "token_color":"<PUNCTUATION>",
	  "value":"]"
	}
      ],
      "00000045": [
	{
	  "column":4,
	  "edges": [],
	  "end":913,
	  "lineno":45,
	  "signature":"",
	  "start":871,
	  "token_color":"<COMMENT>",
	  "value":"#- { @printx ref/imports PRINTX_imports? }"
	}
      ],
      "00000046": [
	{
	  "column":0,
	  "edges": [],
	  "end":918,
	  "lineno":46,
	  "signature":"@914:918<from>",
	  "start":914,
	  "token_color":"<KEYWORD>",
	  "value":"from"
	},
	{
	  "column":8,
	  "edges": [],
	  "end":919,
	  "lineno":46,
	  "signature":"",
	  "start":918,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":5,
	  "edges": [],
	  "end":920,
	  "lineno":46,
	  "signature":"",
	  "start":919,
	  "token_color":"<PUNCTUATION>",
	  "value":"."
	},
	{
	  "column":6,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref/imports",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t9"
	      }
	    }
	  ],
	  "end":922,
	  "lineno":46,
	  "signature":"@920:922<t9>",
	  "start":920,
	  "token_color":"<BARE>",
	  "value":"t9"
	},
	{
	  "column":12,
	  "edges": [],
	  "end":923,
	  "lineno":46,
	  "signature":"",
	  "start":922,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":9,
	  "edges": [],
	  "end":929,
	  "lineno":46,
	  "signature":"@923:929<import>",
	  "start":923,
	  "token_color":"<KEYWORD>",
	  "value":"import"
	},
	{
	  "column":19,
	  "edges": [],
	  "end":930,
	  "lineno":46,
	  "signature":"",
	  "start":929,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":16,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.printx"
	      }
	    },
	    {
	      "edge":"/kythe/edge/ref/imports",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t9.printx"
	      }
	    }
	  ],
	  "end":936,
	  "lineno":46,
	  "signature":"@930:936<printx>",
	  "start":930,
	  "token_color":"<VAR_BINDING>",
	  "value":"printx"
	}
      ],
      "00000048": [
	{
	  "column":4,
	  "edges": [],
	  "end":968,
	  "lineno":48,
	  "signature":"",
	  "start":938,
	  "token_color":"<COMMENT>",
	  "value":"#- { @printx ref PRINTX_var? }"
	}
      ],
      "00000049": [
	{
	  "column":4,
	  "edges": [],
	  "end":1013,
	  "lineno":49,
	  "signature":"",
	  "start":969,
	  "token_color":"<COMMENT>",
	  "value":"#- { PRINTX_var./pykythe/type PRINTX_type? }"
	}
      ],
      "00000050": [
	{
	  "column":4,
	  "edges": [],
	  "end":1046,
	  "lineno":50,
	  "signature":"",
	  "start":1014,
	  "token_color":"<COMMENT>",
	  "value":"#- { @foo defines/binding FOO? }"
	}
      ],
      "00000051": [
	{
	  "column":4,
	  "edges": [],
	  "end":1081,
	  "lineno":51,
	  "signature":"",
	  "start":1047,
	  "token_color":"<COMMENT>",
	  "value":"#- { FOO./pykythe/type FOO_type? }"
	}
      ],
      "00000052": [
	{
	  "column":0,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.foo"
	      }
	    }
	  ],
	  "end":1085,
	  "lineno":52,
	  "signature":"@1082:1085<foo>",
	  "start":1082,
	  "token_color":"<VAR_BINDING>",
	  "value":"foo"
	},
	{
	  "column":7,
	  "edges": [],
	  "end":1086,
	  "lineno":52,
	  "signature":"",
	  "start":1085,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":4,
	  "edges": [],
	  "end":1087,
	  "lineno":52,
	  "signature":"",
	  "start":1086,
	  "token_color":"<PUNCTUATION>",
	  "value":"="
	},
	{
	  "column":9,
	  "edges": [],
	  "end":1088,
	  "lineno":52,
	  "signature":"",
	  "start":1087,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":6,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.printx"
	      }
	    }
	  ],
	  "end":1094,
	  "lineno":52,
	  "signature":"@1088:1094<printx>",
	  "start":1088,
	  "token_color":"<VAR_REF>",
	  "value":"printx"
	}
      ],
      "00000055": [
	{
	  "column":0,
	  "edges": [],
	  "end":1101,
	  "lineno":55,
	  "signature":"@1097:1101<from>",
	  "start":1097,
	  "token_color":"<KEYWORD>",
	  "value":"from"
	},
	{
	  "column":8,
	  "edges": [],
	  "end":1102,
	  "lineno":55,
	  "signature":"",
	  "start":1101,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":5,
	  "edges": [],
	  "end":1103,
	  "lineno":55,
	  "signature":"",
	  "start":1102,
	  "token_color":"<PUNCTUATION>",
	  "value":"."
	},
	{
	  "column":6,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref/imports",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t9"
	      }
	    }
	  ],
	  "end":1105,
	  "lineno":55,
	  "signature":"@1103:1105<t9>",
	  "start":1103,
	  "token_color":"<BARE>",
	  "value":"t9"
	},
	{
	  "column":12,
	  "edges": [],
	  "end":1106,
	  "lineno":55,
	  "signature":"",
	  "start":1105,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":9,
	  "edges": [],
	  "end":1112,
	  "lineno":55,
	  "signature":"@1106:1112<import>",
	  "start":1106,
	  "token_color":"<KEYWORD>",
	  "value":"import"
	},
	{
	  "column":19,
	  "edges": [],
	  "end":1113,
	  "lineno":55,
	  "signature":"",
	  "start":1112,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":16,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.SomeClass"
	      }
	    },
	    {
	      "edge":"/kythe/edge/ref/imports",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t9.SomeClass"
	      }
	    }
	  ],
	  "end":1122,
	  "lineno":55,
	  "signature":"@1113:1122<SomeClass>",
	  "start":1113,
	  "token_color":"<VAR_BINDING>",
	  "value":"SomeClass"
	}
      ],
      "00000057": [
	{
	  "column":4,
	  "edges": [],
	  "end":1170,
	  "lineno":57,
	  "signature":"",
	  "start":1124,
	  "token_color":"<COMMENT>",
	  "value":"#- { @some_class defines/binding SOME_CLASS? }"
	}
      ],
      "00000058": [
	{
	  "column":4,
	  "edges": [],
	  "end":1219,
	  "lineno":58,
	  "signature":"",
	  "start":1171,
	  "token_color":"<COMMENT>",
	  "value":"#- { SOME_CLASS./pykythe/type SOME_CLASS_type? }"
	}
      ],
      "00000059": [
	{
	  "column":0,
	  "edges": [
	    {
	      "edge":"/kythe/edge/defines/binding",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.some_class"
	      }
	    }
	  ],
	  "end":1230,
	  "lineno":59,
	  "signature":"@1220:1230<some_class>",
	  "start":1220,
	  "token_color":"<VAR_BINDING>",
	  "value":"some_class"
	},
	{
	  "column":14,
	  "edges": [],
	  "end":1231,
	  "lineno":59,
	  "signature":"",
	  "start":1230,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":11,
	  "edges": [],
	  "end":1232,
	  "lineno":59,
	  "signature":"",
	  "start":1231,
	  "token_color":"<PUNCTUATION>",
	  "value":"="
	},
	{
	  "column":16,
	  "edges": [],
	  "end":1233,
	  "lineno":59,
	  "signature":"",
	  "start":1232,
	  "token_color":"<WHITESPACE>",
	  "value":" "
	},
	{
	  "column":13,
	  "edges": [
	    {
	      "edge":"/kythe/edge/ref",
	      "target": {
		"corpus":"CORPUS",
		"language":"python",
		"path":"",
		"root":"ROOT",
		"signature":".home.peter.src.pykythe.test_data.t10.SomeClass"
	      }
	    }
	  ],
	  "end":1242,
	  "lineno":59,
	  "signature":"@1233:1242<SomeClass>",
	  "start":1233,
	  "token_color":"<VAR_REF>",
	  "value":"SomeClass"
	}
      ],
      "00000060": []
    },
    "path":"home/peter/src/pykythe/test_data/t10.py",
    "root":"ROOT"
  },
]
;
