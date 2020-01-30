# Replica

[![CircleCI](https://circleci.com/gh/pkamenarsky/replica.svg?style=svg)](https://circleci.com/gh/pkamenarsky/replica)

![Remote logo](./docs/replica-logo.svg)

**Replica** is a *remote virtual DOM* library for Haskell. In contrast to traditional virtual DOM implementations, a remote virtual DOM runs *on the server* and is replicated to the client, which just acts as a dumb terminal.

![Remote DOM](./docs/replica-dom.svg)

## Quick start

To see it in action, check out the [list of **Replica** frameworks](#list-of-replica-frameworks)

## Motivation

SPAs written in frameworks such as [React](https://reactjs.org) or [Vue](https://vuejs.org) often require setting up projects in different languages with their respective build tooling; thinking about protocols between server and client; designing data types for communication (whether explicitly or implicitly) and coming up with a way to keep everything in sync reliably. More often than not this results in copious amounts of boilerplate and support code completely unrelated to the task at hand.

A change on the backend has to be propagated through the server code and protocol or API long before the affected UI components can be adjusted. Conversely, changes in the UI specification or requirements depend upon long-winded thought experiments in data flow before the required modifications on the backend can be pinned down. Prototyping speed slows down and maintenance costs go up.

And so, one often finds oneself longing for the Olden Days of server-side HTML rendering, where all the data was readily available at our fingertips without protocols or different ecosystems to deal with. However, this comes at the cost of losing out on designing UIs declaratively in the spirit of [Elm](https://elm-lang.org) or [React](https://reactjs.org) and of course on the interactivity displayed by code running directly on the client side.

### Remote virtual DOM

**Replica** seeks the middle ground – it runs a virtual DOM *remotely on the server* while pushing only minimal diffs to the client, which in turn sends events triggered by the user back to the backend. This offers all the advantages of generating a complete UI on the server, while still providing the interactivity of more traditional SPAs running entirely on the client.

A word about data volume – currently, the data format is not optimised for size at all; that said, it should be comparable to a finely hand-tuned protocol, since only the most minimal changeset is sent over the wire (save for a small-ish constant overhead because of the ceremony caused by the recursive tree nature of the data). In contrast, many APIs are constructed to send the whole requested dataset on every change, so **Replica**'s diffs might fare favourably here. To that point, designing a protocol with minimal data volumes in mind is however also very likely to increase the volume of the aforementioned incidental boilerplate and support code.

### Preventing common errors

There is also the additional burden of thinking about sensitive data – a simple `select * from users` would also send the (hopefully) hashed user passwords back to the client, even though they are not displayed anywhere in the UI. Thinking hard about scrubbing records before throwing something over the wire is laborious and error prone, and humans make mistakes.

With a remote VDOM, *all code* runs on the backend – no need to come up with ways to share functionality between server and client, to validate forms twice or authenticate API requests.

That is not to say that **Replica** itself (or its programming model) is more secure than any other program exposed to a network, but a certain class of common errors stemming from the split between back- and frontend are presumably more difficult to make.

### Bundle sizes

Megabyte-sized code bundles including hundreds of dependencies can certainly take a bit to download, however parsing and compiling JavaScript is often just as slow. With a remote VDOM, *no code at all* is shipped to the client (except for the JS driver), only diffs and assets like CSS, images, or fonts. Although there's no support for server-side HTML rendering yet, once implemented it will be just as easy to push everything at once (without having to wait for the driver to establish a WebSocket connection and request an initial DOM update), reducing the time to first render even further.

### Testing

Since there is no dependency on a browser environment, simulating UI interactions is just a matter of getting hold of the VDOM and firing a synthesized event on a particular node. QuickChecking components or even [metamorphic testing](http://www.lsi.us.es/~segura/files/papers/segura17-tse.pdf) are certainly some interesting avenues to explore in the context of UI testing.

## Client side prediction

**Replica** runs over a WebSocket connection. A virtual DOM wired up with events which might lead to change needs to react to every such event – be it a mouse click or a key stroke. Normally, even on an average connection, some lag between a click on a button and showing the updated UI is fine; not so when it comes to typing. Even a lag of ~50ms becomes noticeable, since the virtual DOM is a complete and accurate representation of the UI displayed to the user and the values of text input elements need to be replicated as well.

Game developers have had to deal with the problem of client side prediction at least since the days of Quake and so the solution space is well understood. For the time being, **Replica**'s algorithm is simple – every event → DOM patch roundtrip increases a frame number on the server and every such patch is tagged with said frame number. Additionally, every input element wired with an event listener keeps its value in a capped queue in the browser DOM for a given number of frames (currently 20). Finally, when the DOM is patched, the input value is not touched if the server value matches any of the previous frame values stored on the client.

Even with a simple scheme like this, the user experience is indistinguishable from code running directly on the client, for the majority of cases, for even higher lag values of ~100–200ms. *Rare* edge cases do show, but those can be mitigated against in the future by employing more sophisticated client-side prediction algorithms.

## Caveats

Since DOM diffing runs on the server, **Replica** is more resource intensive than a comparable backend implementation which just returns data without diffing. It's not recommended to use it for high-traffic user-facing applications. It might however be the perfect fit for internal tooling where in many cases prototyping and maintenance costs trump hardware prices by a wide margin.

Additionally, events such as `onMouseMove` are discouraged, even though they are supported in principle. This is because there might be better ways to provide high interactivity in the future – for example, by implemeting a custom, highly optimised `onMouseDrag` event – than bombarding the server with a torrent of movement events.

There's no support for animations and lifecycle events yet, but the implementation would be relatively straightforward.

## Building

Install [TypeScript](https://www.typescriptlang.org) and [Stack](https://docs.haskellstack.org/en/stable/README). Then:

```
cd js && tsc --project tsconfig.json && cd ..
stack build --test
```

The TypeScript step must be executed whenever `js/client.ts` changes.

## Integration with UI frameworks

**Replica** aims to be framework-agnostic. It has a simple API and hooking into it should be as uncomplicated as possible.

The easiest way to run **Replica** is to call `Network.Wai.Handler.Replica.app`. This takes care of distributing a complete `index.html` together with the JavaScript driver to the client. Everything should work out of the box.

For finer grained integration, there's `Network.Wai.Handler.Replica.websocketApp`, which just handles the WebSocket part of the connection; the root `js/index.html` and `js/dist/client.js` must be distributed separately.

`Replica.VDOM.diff` and `Replica.VDOM.fireEvent` allow for complete control over the remote virtual DOM.

## List of **Replica** frameworks

* [`concur-replica`](https://github.com/pkamenarsky/concur-replica)

## Roadmap

* Hackage documentation
* Initial server-side rendering
* ~SVG support~ ([@seagreen](https://github.com/seagreen))
* Better diffing algorithm
* Lifecycle events and animation hooks
* `onMouseDrag` event
* Nix derivation
* QuickCheck component framework

## Bugs and features

Comments, bug reports and feature PRs very much welcome.
