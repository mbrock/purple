document.querySelectorAll("pre").forEach(function(x) {
  x.innerHTML = x.innerHTML
    .replace(/^(type |newtype |data )?(\w+)(\b.+=(?: |$))/gm, "$1<define id=dfn-$2>$2</define>$3")
    .replace(/^(data )(\w+)$/gm, "$1<define id=dfn-$2>$2</define>")
    .replace(/^(\s+_)(\w+)(\s+::)/gm, "$1<define id=dfn-$2>$2</define>$3")
    .replace(/^(\s+(?:\| |= ))(\w+)(?= .*?--)/gm, "$1<define id=dfn-$2>$2</define>")
})

var words = []
document.querySelectorAll("define").forEach(function(x) {
  words.push(x.innerText)
})

var re1 = new RegExp("([ [(])(" + words.join("|") + ")\\b", "g")
var re2 = new RegExp("\\b(" + words.join("|") + ")\\b", "g")

document.querySelectorAll("pre,  code").forEach(function(x) {
  x.innerHTML = x.innerHTML
    .replace(re1, "$1<a href=\"#dfn-$2\">$2</a>")
    .replace(/\b(module|import|data|newtype|type|deriving|if|case|where|let|do)\b/g, "<b>$&</b>")
    .replace(/^(\s*)-- (.*)/gm, "$1<comment class=block>$2</comment>")
    .replace(/(\s+)-- (.*)/g, "$1<comment>$2</comment>")
    .replace(/\$([^ ]+)\$/g, "<math>$1</math>")
    .replace(/ \. /g, " ∘ ")
    .replace(/\b_(\w+)\b/g, "· $1")
    .replace(/\b_(<\w+)\b/g, "· $1")
    .replace(/^(\w+ :: .*)$/gm, "<declaration>$1</declaration>")
    .replace(/\b(\w+)_(\w+)\b/g, "$1<sub>$2</sub>")
    .replace(/\b([a-z]+)([01])\b/g, "$1<sub>$2</sub>")
    .replace(/ -&gt; /g, " → ")
    .replace(/ &lt;- /g, " ← ")
    .replace(/\n\s*\n/gm, "<p>")
})

document.querySelectorAll("p > code").forEach(function(x) {
  x.innerHTML = x.innerHTML
    .replace(re2, " <a href=\"#dfn-$1\">$1</a>")
})

var counters = []
var ToC = []

document.querySelectorAll("h1, h2, h3, h4").forEach(function(x) {
  var n = +(x.tagName.substr(1, 1))
  counters = counters.slice(0, n)
  if (counters.length < n) {
    counters = counters.concat(new Array(n - counters.length).fill(1))
  } else {
    counters[n - 1]++
  }
  x.id = "sec-" + counters.join("-")
  ToC.push({ counters: counters, title: x.innerText })
  x.innerHTML = "<a href=#" + x.id + ">" + counters.join(".") + "</a>&nbsp;&nbsp;" + x.innerHTML
})

var toc = document.querySelector("toc")
ToC.forEach(function (x) {
  var e = document.createElement("toc-entry")
  var e1 = document.createElement("toc-n")
  e1.innerText = x.counters.join(".")
  var e2 = document.createElement("toc-title")
  e2.innerHTML = "<a href=#sec-" + x.counters.join("-") + ">" + x.title + "</a>"
  if (x.counters.length == 1)
    e.classList.add("woah")
  if (x.counters.length == 1)
    e2.classList.add("bold")
  e.appendChild(e1)
  e.appendChild(e2)
  toc.appendChild(e)
})