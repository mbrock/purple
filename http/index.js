const url = "http://localhost:1988"

let $ = React.createElement
let dec = s => new Decimal(s)
let unray = x => x.toFixed(36)
let unwad = x => x.toFixed(18)

let yearSecs = dec("3.154e7")
let wayYearPps = way => way.pow(yearSecs).sub(1).mul(dec(100))
let showWay = way => `${wayYearPps(way).toFixed(5)} pp/year`
let showHow = how => `${showWay(how)}²`

let transform = ({
  balances,
  vat: {
    vox: { way, par, wut, how, tau },
    tags, ilks, urns,
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
    tags: tags.map(([id, {
      tag, zzz
    }]) => [id, {
      tag: dec(tag),
      zzz: dec(zzz),
    }]),
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
    tags, ilks, urns,
  }
}) => $("div", null, [
  ...renderTable(
    "vox",
    [null],
    "par wut way how tau".split(" "),
    _ => [
      $("td", null, par.toString()),
      $("td", null, wut.toString()),
      $("td", null, showWay(way)),
      $("td", null, showHow(how)),
      $("td", null, tau.toString()),
    ]
  ),
  ...renderTable(
    "ilks",
    ilks,
    "ilk axe hat lax mat tax chi rho rum".split(" "),
    ([id, ilk]) => [
      $("td", null, id),
      $("td", null, ilk.axe.toString()),
      $("td", null, ilk.hat.toString()),
      $("td", null, ilk.lax.toString()),
      $("td", null, ilk.mat.toString()),
      $("td", null, ilk.tax.toString()),
      $("td", null, ilk.chi.toString()),
      $("td", null, ilk.rho.toString()),
      $("td", null, ilk.rum.toString()),
    ]
  ),
  ...renderTable("urns", urns, "urn lad ilk ink art cat".split(" "), ([id, urn]) => [
    $("td", null, id),
    $("td", null, urn.lad),
    $("td", null, urn.ilk.toString()),
    $("td", null, urn.ink.toString()),
    $("td", null, urn.art.toString()),
    $("td", null, urn.cat),
  ]),
  ...renderTable("tags", tags, ["gem", "tag", "zzz"], ([id, { tag, zzz }]) => [
    $("td", null, id.tag == "Gem" ? id.contents : id.tag),
    $("td", null, tag.toString()),
    $("td", null, zzz.toString()),
  ]),
  ...renderTable("wads", balances, ["lad", "gem", "wad"], x => [
    $("td", {
      className: x[0][0].contents ? null : "special"
    }, x[0][0].contents || x[0][0].tag),
    $("td", null, x[0][1].contents || x[0][1].tag),
    $("td", null, x[1].toString()),
  ]),
])

let renderTable = (title, xs, ths, f) => !xs.length ? [] : [
  $("div", { className: "box" }, [
    $("table", null, [
      $("thead", null, [
        $("tr", null, [
          $("th", null, title),
          ...ths.map(s => $("th", null, s))
        ])
      ]),
      $("tbody", null, xs.map(x => $("tr", null, [
        $("td", null, ""),
        ...f(x)
      ])))
    ])
  ])
]

let render = ({
  saga,
}) => $("ol", null, [
  saga.map(([act, system]) =>
    $("li", null, [
      $("explanation", null, explainAct(act)),
      renderAct(act),
      renderSystem(system),
    ])
  )
])

let opts = args =>
  Object.keys(args).map(k => `--${k}/${args[k]}`).join("/")

let explainAct = act => {
  let parts = act.split("/")
  let verb = parts[0]
  let arg = s => parts[parts.indexOf(`--${s}`) + 1]
  let cases = {
    init: () => `Initialize the system.`,
    frob: () => `Set sensitivity to ${arg("how")}.`,
    mint: () => `Create ${arg("wad")} ${arg("gem")} in account of ${arg("lad")}.`,
    form: () => `Create ilk ${arg("ilk")} with gem ${arg("gem")}.`,
    cork: () => `Set hat of ilk ${arg("ilk")} to ${arg("hat")}.`,
    mark: () =>
      `Mark gem ${arg("gem")} with price ${arg("tag")} XDR, valid ${arg("zzz")}s.`,
  }
  return cases[verb] ? cases[verb]() : ""
}

let renderAct = act => {
  let parts = act.split("/")
  return $("span", { className: "act" }, [
    $("b", null, parts[0]),
    ...(parts.slice(1).map(
      x => $(
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
