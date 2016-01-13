# clj-money

This is a web-based implementation of a basic [double-entry accounting system](https://en.wikipedia.org/wiki/Double-entry_bookkeeping_system).
A basic working knowledge of these accounting principles will be necessary to make sense of the site.

There is also a Ruby on Rails version of exactly the same application [here](https://github.com/dgknght/money), for those of you that like to compare and contrast.

## Running locally

### Source Code
Clone the repo

```
git clone git@github.com:dgknght/clj-money.git
```

### Data Storage
You'll need [Datomic](http://www.datomic.com/). You can download a free version [here](https://my.datomic.com/downloads/free). Follow their instructions for setting it up.

Start an instance of the transactor by executing the following command in a terminal window.
```
<path-to-datomic>\transactor <path-to-this-project>\config\transactor.properties
```

### Start the service
To start the service, simple start a repl from within the project.
```
lein repl
```

**NOTE** *I need to rework the service so that you can start it up  and close it down from the repl. One thing at a time, though.*

### Initialize the Data Store
Within the repl, execute the following:
```
(clj-money.admin/init-database)
```
This will create the database and apply the schema.

### Open a browser
Simply navigate to (http://localhost:3204/) and start accounting.


## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
