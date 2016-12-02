'use strict';

require('../styles/main.styl');
require('../styles/header.styl');

var profiles =
  [ require('../markdown/dennisreimann/profile.markdown')
  , require('../markdown/bastiankrol/profile.markdown')
  ]

var Elm = require('../elm/Main');
var node = document.getElementById('app');
var app = Elm.Main.embed(node, { profiles: profiles });
