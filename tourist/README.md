# 📸 tourist

> “A good tourist does not trust the map, but instead walks every street.”  
> —  LLM hallucination

**tourist** is a Haskell program that goes on a trip through your SPA and comes back with a suitcase full of static HTML and CSS.

It visits your site, waits for everything to render, and turns it into static output you can deploy anywhere.

## What?

Instead of trying to understand your frontend, **tourist** just ... visits it.

- 🌍 Crawls your site like a curious human
- 🧠 Keeps track of where it's been (no double visits)
- 🎭 Uses a real browser (powered by [Dramaturge])
- 📄 Exports clean, static HTML
- 🎨 Collects CSS and other assets
- ⚙️ Lets you plug in custom actions (screenshots, scraping, etc.)

## Why?

SPAs are great to build, but often overkill to serve.

If you're serving a homepage, a blog, or documentation, your users probably don't need a live JavaScript application in the browser.
What you actually want is the final result: fast-loading, portable, static files.

Most developers don't want to give up their existing tools.
If you're already comfortable building apps with something like [Miso] or [React], switching to a traditional static site generator can feel like a step backwards.

**tourist** lets you keep your workflow.

Build your site however you like - as a fully dynamic SPA - and let tourist handle the last mile.

## How?

Behind the scenes, **tourist** is implemented as an integration test using Dramaturge.

Dramaturge instruments a Firefox browser via the Marionette protocol.
The browser can be either headless or visible, if you want to tag along for the ride.

The test process goes as follows:
1. Put all user-provided pages into a queue
2. Pop a page from the queue and visit it
3. Wait for the page to populate (content inside `<body>`)
4. Extract fully rendered HTML, screenshots, assets, ...
5. Collect links to other local pages that have not yet been visited and put them into the queue
6. Repeat from step 2 until the queue is empty

## Usage

```
tourist [OPTIONS] [URL]...
```

| Option                          | Description                                     |
| ------------------------------- | ----------------------------------------------- |
| `--headless`/`--graphical`      | Run with or without a GUI (default: headless)   |
| `--quiet`/`--verbose`/`--debug` | Control log output (default: verbose)           |
| `--output <DIR>`                | Where to write the static files (default: `.`)  |
| `--firefox <FILE>`              | Path to the Firefox binary (default: `firefox`) |

**tourist** likes to travel light; it does not download any browsers or driver binaries to your computer.
All it needs is Firefox to hitch a ride in.

If using [Nix], you don't even need Firefox. Just run:

```
nix run github:ners/dramaturge#tourist -- [OPTIONS] [URL]...
```

[Dramaturge]: https://github.com/ners/dramaturge
[Miso]: https://haskell-miso.org/
[React]: https://react.dev/
[Nix]: https://nixos.org/
