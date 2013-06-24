Available via [clojars](http://clojars.org/search?q=bagotricks)   
Current stable version: [bagotricks "1.5.2"]


# Bagotricks

A collection of miscellaneous useful Clojure functions pulled from a various different sources. 


## Usage
```clojure

;; Highlights;

(to-long "5") ;; Coerces value provided to a long.

(->> (range 400)
     (clojure.core.reducers/map inc)
     fold-into-vec) ;; Fold the results of a reducer into a vector. 

(->> (range 400)
     (clojure.core.reducers/map vector)
     fold-into-lazy-seq
     dump-recs-to-file) ;; Fold the results of a reducer into a lazy sequence, and write to file (order not maintained).

(read-lines filename) ;; returns a vector where each element is a line in the file.

(re-get #"a (b) c" "a b c") ;; returns just ("b")

(split-tsv "a\tb\t\td") ;; returns ["a" "b" nil "d"]

(thread (println "this is executing in another thread!")) ;; Simply execute code in another thread (good for side effects).

;; etc.
```


## Artifacts

Bagotricks artifacts are [released to Clojars](https://clojars.org/bagotricks).

If you are using Maven, add the following repository definition to your `pom.xml`:

``` xml
<repository>
  <id>clojars</id>
  <url>http://clojars.org/repo</url>
</repository>
```

### The Most Recent Release

With Leiningen:

    [bagotricks "1.5.2"]


With Maven:

    <dependency>
      <groupId>bagotricks</groupId>
      <artifactId>bagotricks</artifactId>
      <version>1.5.2</version>
    </dependency>


## License

CC0
http://creativecommons.org/publicdomain/zero/1.0/

I'd also like to thank my employer Gracenote, for allowing me to release this.

NOTE: Relevant bits of bagotricks.clj are pulled from a number of sources, and credit goes to the original author. Details associated with each particular function.

Copyright (C) 2012-2013 Alan Busby

