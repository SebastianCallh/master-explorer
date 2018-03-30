# Master Explorer
Master explorer is a web application for intelligently planning studies at Linköping university. It achieves this by offering a visualization of the availible courses, course blocks, displaying overlapping corses, calculating selected amount of credits and similair helpful things. 

## Functional requirement
- The user shall be able to select courses 
- The user shall be able to deselect courses
- The user shall be able to see in which blocks selected courses are
- The user shall be able to see the number of credits on selected courses
- The user shall be able to see requiremnets for completing a course, such as lab, written exam och home exam

## Technical Details
The application is build in `Haskell`. A big benefit of using the same language across the entire project is  that there is no need to manually marshall data. It can be serialized under the hood by well tested frameworks. `Haskell`s fantastic type system also gives very strong compile-time guarantees that the application won't blow up in production, across both server and client. 

Since `Haskell` is a bit of an oddball when it comes to language choices I have tried to provide links to the `Hackage` page of all relevant packages used (`Hackage` is the central package archive for the `Haskell` community). Should documentation be lacking there are typically links to projects `GitHub` pages where it is usually more fleshed out. 

### Server
The server code sits on top of the web server [`Warp`](https://hackage.haskell.org/package/warp) which is wrapped by [`WAI`](https://hackage.haskell.org/package/wai). On top of this sits the actuall application which uses the API framework [`Servant`](https://hackage.haskell.org/package/servant) to specify RESTful endpoints to the server. The server uses a `PostgreSQL` instance for storage which is wrapped by [`Persistent`](https://hackage.haskell.org/package/persistent) that also handles database migrations. Queries to the database is written in the DSL [`Esqueleto`](https://hackage.haskell.org/package/esqueleto), allowing `Haskell`s type safety to reach as far as into the database. 

#### Motivation
`Servant` is a very well supported package for declaring APIs, and so is `Persisten` for databases. A web server is of course needed for them to sit on and in that department `Warp` is like the previous packages very well supported together with `Wai`.

### Client
The client is built using the FRP (Functional reactive programming) framework [Reflex](https://hackage.haskell.org/package/reflex-dom) together with [Reflex-DOM](https://hackage.haskell.org/package/reflex) for DOM manipulation.

#### Motivation
FRP offers an interesting way of structuring user interfaces in a functional way and `Reflex` and `Servant` can share data, letting `Haskell` type check the communication between client and server at compile time. At this point all code between the database and the client is type checked, making it almost impossible to get a runtime crash.

### Scraper
To collect course data the [Liu course page](https://liu.se/studieinfo) has to be scraped (since LiU IT did not provide me with an API upon asking). This is done by running a scraper with a cron job at regular intervals. 
