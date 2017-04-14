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
  balances: balances.map(([[lad, gem], wad]) => [
    [lad, gem], dec(wad)
  ]),
  vat: {
    vox: {
      way: dec(way),
      par: dec(par),
      wut: dec(wut),
      how: dec(how),
      tau: dec(tau),
    },
    jars,
    ilks: ilks.map(([id, {
      axe, hat, lax, mat, tax, chi, rho, rum
    }]) => [id, {
      axe: dec(axe),
      hat: dec(hat),
      lax: dec(lax),
      mat: dec(mat),
      tax: dec(tax),
      chi: dec(chi),
      rho: dec(rho),
      rum: dec(rum),
    }]),
    urns: urns.map(([id, {
      lad, ilk, art, ink, cat
    }]) => [id, {
      lad: lad.contents,
      ilk,
      art: dec(art),
      ink: dec(ink),
      cat,
    }])
  }
})

let renderSystem = ({
  balances,
  vat: {
    vox: { way, par, wut, how, tau },
    jars, ilks, urns,
  }
}) => tag("div", null, [
  ...renderTable(
    ilks,
    "ilk axe hat lax mat tax chi rho rum".split(" "),
    ([id, ilk]) => [
      tag("td", null, id),
      tag("td", null, ilk.axe.toString()),
      tag("td", null, ilk.hat.toString()),
      tag("td", null, ilk.lax.toString()),
      tag("td", null, ilk.mat.toString()),
      tag("td", null, ilk.tax.toString()),
      tag("td", null, ilk.chi.toString()),
      tag("td", null, ilk.rho.toString()),
      tag("td", null, ilk.rum.toString()),
    ]
  ),
  ...renderTable(urns, "urn lad ilk ink art cat".split(" "), ([id, urn]) => [
    tag("td", null, id),
    tag("td", null, urn.lad),
    tag("td", null, urn.ilk.toString()),
    tag("td", null, urn.ink.toString()),
    tag("td", null, urn.art.toString()),
    tag("td", null, urn.cat),
  ]),
  ...renderTable(balances, ["lad", "gem", "wad"], x => [
    tag("td", {
      className: x[0][0].contents ? null : "special"
    }, x[0][0].contents || x[0][0].tag),
    tag("td", null, x[0][1].contents || x[0][1].tag),
    tag("td", null, x[1].toString()),
  ]),
])

let renderTable = (xs, ths, f) => !xs.length ? [] : [
  tag("table", { className: "box" }, [
    tag("thead", null, [tag("tr", null, ths.map(s => tag("th", null, s)))]),
    tag("tbody", null, xs.map(x => tag("tr", null, f(x))))
  ])
]

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
