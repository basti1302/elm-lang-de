'use strict';

require('!style!css!spectre.css/dist/spectre.min.css');
require('!style!css!font-awesome/css/font-awesome.min.css');
require('../styles/main.styl');
require('../styles/header.styl');
require('../styles/homepage.styl');
require('../styles/profiles.styl');

var Elm = require('../elm/Main');
var node = document.getElementById('app');
var app = Elm.Main.embed(node);
