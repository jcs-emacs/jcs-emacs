/**
 * $File: index.js $
 * $Date: 2020-08-30 16:02:11 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2020 by Shen, Jen-Chieh $
 */

"use strict";

function display() {
  showdown.setFlavor('github');
  let converter = new showdown.Converter();
  converter.setOption('simpleLineBreaks', false);

  let md = document.querySelector('#markdown-body div');
  if (!md)
    return;

  let text = md.innerHTML;
  let newHTML = converter.makeHtml(text);

  md.innerHTML = newHTML;
}
