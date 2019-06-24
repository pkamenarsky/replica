# Replica

[![CircleCI](https://circleci.com/gh/pkamenarsky/replica.svg?style=svg)](https://circleci.com/gh/pkamenarsky/replica)

**Replica** is a *remote virtual DOM* library for Haskell. In contrast to traditional virtual DOM implementations, a remote virtual DOM runs *on the server* and is replicated to the client, which just acts as a dumb terminal.

See [list of **Replica** frameworks](list-of-replica-frameworks).

## Motivation

SPAs written in frameworks such as [React](https://reactjs.org) or [Vue](https://vuejs.org) oftentimes require setting up projects in different languages with their respective build tooling, thinking about protocols between server and client, designing data types for communication (whether explicitly or implicitly) and coming up with a way to keeping everything in sync reliably. More often than not this results in copious amounts of boilerplate and support code completely unrelated to the task at hand.

A change on the backend has to be propagated through the server code and protocol or API long before the affected UI components can be adjusted. Conversely, a specification or requirements change in the UI depend upon long-winded thought experiments in data flow before the required modifications on the backend can be pinned down. Prototyping speed grinds to a halt and maintenance costs skyrocket.

There is also the additional burden of thinking about security - a simple `select * from users` would also send the (hopefully) hashed user passwords back to the client, even though they are not displayed anywhere in the UI. Thinking hard about scrubbing sensitive data before throwing something over the wire is laborious and error prone and humans make mistakes.

And so, one often finds oneself longing for the Olden Days of server side HTML rendering, where all the data was readily available at one's fingertips without protocols or different ecosystems to be taken care of. However, one then looses out on designing UIs declaratively in the spirit of [Elm](https://elm-lang.org) or [React](https://reactjs.org) and of course on the interactivity offered by code running directly on the client side.

**Replica** seeks the middle ground - it runs a virtual DOM *remotely on the server* while pushing only minimal diffs to the client, which in turn sends events triggered by the user back to the backend. This offers all of the advantages of generating a complete UI on the server, while still providing the interactivity of more traditional SPAs running entirely on the client.

A word about data volume - currently, the data format is not optimised for size at all; that said, it should be comparable to a finely hand-tuned protocol, since only the most minimal changeset is sent over the wire (save for a small-ish volume constant because of the ceremony caused by the recursive tree nature of the data). In contrast, many APIs are constructed to send the whole requested dataset on every change, so **Replica**'s diffs might fare favourably here. Furthermore, designing a protocol with minimal data volumes in mind would probably also increase the volume of the aforementioned incidental boilerplate and support code.

## Client side prediction

**Replica** runs over a WebSocket connection. A virtual DOM wired up with events which might lead to change needs to react to every such event - be it a mouse click or a key stroke. Normally, even on an average connection, some lag between a click on a button and showing the updated UI is fine; not so when it comes to typing. Even a lag of ~50ms starts to be noticeable, since the DOM is a complete and accurate representation of the UI displayed to the user and the values of text input elements need to be replicated as well.

Game developers have had to deal with the problem of client side prediction at least since the days of Quake and so the solution space is well understood. **Replica**, for the time being, offers a simple implementaion - every event -> DOM patch roundtrip increases a frame number on the server and every such patch is tagged with said frame number. Additionally, every input element wired with an event listener keeps its value in a capped queue in the browser DOM for a given number of frames (currently 20). Finally, when the DOM is patched, the input value is not touched iff the server value matches any of the previous frame values stored on the client.

Even with a simple scheme like this the user experience is indistinguishable from code running directly on the client, for the majority of cases, for even higher lag values of ~100ms - 200ms. *Rare* edge cases show, but those can be mitigated in the future by employing more sophisticated client side prediction algorithms.

## Caveats

Since DOM diffing runs on the server, **Replica** is relatively more resource intensive than a comparable backend implementation which just servers data without diffing. It's not recommended to use it for high-traffic user facing applications. However, it might be the perfect fit for internal tooling where in many cases prototyping and maintenance costs trump hardware prices by a long margin.

Additionally, events such as `onMouseMove` are discouraged, although they are supported in principle. This is because there might be better ways to provide high interactivity in the future (for example, by implemeting a custom, highly optimised `onMouseDrag` event) than bombarding the server with a torrent of movement events.

There's no support for animations and lifecycle events yet, however the implementation would be relatively straightforward.

## Building

Install [TypeScript](https://www.typescriptlang.org) and [Stack](https://docs.haskellstack.org/en/stable/README). Then:

```
cd js && tsc --project tsconfig.json && cd ..
stack build --test
```

The TypeScript step must be executed whenever `js/client.ts` changes.

## Integration with UI frameworks

**Replica** aims to be framework agnostic. It offers a simple API and hooking into it should be as uncomplicated as possible. That said, feedback is very much welcome.

## List of **Replica** frameworks

* [`concur-replica`](https://github.com/pkamenarsky/concur-replica)

## Roadmap

* Better documentation
* Initial server-side rendering
* SVG support
* Better diffing algorithm
* Lifecycle events and animation hooks
* `onMouseDrag` event

## Bugs and features

Bug reports and feature PRs very much welcome.
