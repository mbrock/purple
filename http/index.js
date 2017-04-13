const url = "http://localhost:1988"

let tag = React.createElement
let dec = s => new Decimal(s)
let unray = x => x.toFixed(36)
let unwad = x => x.toFixed(18)

let yearSecs = dec("3.154e7")
let wayYearPps = way => way.pow(yearSecs).sub(1).mul(dec(100))
let showWay = way => `${wayYearPps(way).toFixed(5)} pp/year`

let transform = ({
  balances,
  vat: {
    vox: { way, par, wut, how, tau },
    jars, ilks, urns,
  } 
}) => ({
  balances,
  vat: {
    vox: {
      way: dec(way),
      par: dec(par),
      wut: dec(wut),
      how: dec(how),
      tau: dec(tau),
    },
    jars,
    ilks,
    urns,
  }
})

let renderSystem = ({
  balances,
  vat: {
    vox: { way, par, wut, how, tau },
    jars, ilks, urns,
  }
}) => tag("div", null, [
  ...(urns.length ? [tag("table", { className: "box" }, [
    tag("thead", null, [
      tag("tr", null, [
        tag("th", null, "urn"),
        tag("th", null, "lad"),
        tag("th", null, "ilk"),
        tag("th", null, "ink"),
        tag("th", null, "art"),
        tag("th", null, "cat"),
      ])
    ]),
    tag("tbody", null, urns.map(([id, urn]) =>
      tag("tr", null, [
        tag("td", null, id),
        tag("td", null, urn.lad.contents),
        tag("td", null, urn.ilk),
        tag("td", null, urn.ink),
        tag("td", null, urn.art),
        tag("td", null, urn.cat),
      ])
    )),
  ])] : []),
  ...(balances.length ? [tag("table", { className: "box" }, [
    tag("thead", null, [
      tag("tr", null, [
        tag("th", null, "lad"),
        tag("th", null, "gem"),
        tag("th", null, "wad"),
      ]),
    ]),
    tag("tbody", null, [
      ...balances.map(x => tag("tr", null, [
        tag("td", {
          className: x[0][0].contents ? null : "special"
        }, x[0][0].contents || x[0][0].tag),
        tag("td", null, x[0][1].contents || x[0][1].tag),
        tag("td", null, x[1]),
      ]))
    ]),
  ])] : [])
])


let render = ({
  saga,
}) => tag("ol", null, [
  saga.map(([act, system]) =>
    tag("li", null, [
      renderAct(act),
      renderSystem(system),
    ])
  )
])

let opts = args =>
  Object.keys(args).map(k => `--${k}/${args[k]}`).join("/")

let renderAct = act => {
  let parts = act.split("/")
  return tag("span", { className: "act" }, [
    tag("b", null, parts[0]),
    ...(parts.slice(1).map(
      x => tag(
        "span", { className: x.startsWith("--") ? "param" : null },
        x.replace(/^--/, "")
      )
    ))
  ])
}

let post = (path, body) =>
  fetch(`${url}/${path}`, {
    method: "POST", body: body ? JSON.stringify(body) : null,
  }).then(x => x.json())

let act = (verb, args) => `${verb}/${opts(args)}`

let saga = acts => {
  let xs = []
  return acts.reduce(
    (p, act) => p.then(x => post(act, x).then(y => (xs.push([act, y]), y))),
    Promise.resolve(null)
  ).then(_ => xs)
}

let state = {
  saga: [],
}

let realize = x => {
  state.saga = x.map(([act, s]) => [act, transform(s)])
  return render(state)
}

saga([
  "init",
  "frob/--how/1.000000001",
  "mint/--lad/mbrock/--gem/ETH/--wad/100",
  "form/--ilk/ETH1/--gem/ETH",
  "cork/--ilk/ETH1/--hat/100",
  "mark/--gem/ETH/--tag/20/--zzz/600",
  "open/--lad/mbrock/--ilk/ETH1/--urn/m",
  "lock/--lad/mbrock/--urn/m/--wad/50",
  "draw/--lad/mbrock/--urn/m/--dai/10",
]).then(
  x => ReactDOM.render(realize(x), document.getElementById("app"))
)

document.write("<div id=app></div>")
