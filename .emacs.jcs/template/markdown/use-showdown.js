/**
 * $File: index.js $
 * $Date: 2020-08-30 16:02:11 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2020 by Shen, Jen-Chieh $
 */

"use strict";

var converter = new showdown.Converter();
converter.setFlavor('github');

var markdownBody = document.getElementById('markdown-body');
var text = markdownBody.innerHTML;
var newHTML      = converter.makeHtml(text);

markdownBody.innerHTML = newHTML;

console.log('Converted from showdown.');
