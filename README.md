# replica

**replica** is a *remote virtual DOM* library for Haskell. In contrast to traditional virtual DOM implementations, a remote DOM runs *on the server* and is replicated on the client, which just acts as a dumb terminal. Specifically:

1. The client serialises and sends relevant events to the server
2. The server computes the new DOM and sends back a diff to the client

## Motivation

* Single language
* No boilerplate setting up serialisation data types and keeping them in sync
* Security, to an extent

## Caveats

* DOM runs on server, resource heavy
* Good for in-house tools without a lot of traffic
* `onMouseMove` and such events are not supported, although a custom `onMouseDrag` event might be implemented soon
* No support for animations and lifecycle methods yet
