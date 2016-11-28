'use strict';

require('../styles/main.styl');

var Elm = require('../elm/Main');
var node = document.getElementById('app');
var app = Elm.Main.embed(node);
