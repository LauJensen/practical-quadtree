# A practical Quadflocking implementation

A [Quadtree](https://en.wikipedia.org/wiki/Quadtree) is a tree data structure, which is useful for giving more focus/detail to certain regions of your data.

I recently wrote a little about [functional quadtrees](https://lbjgruppen.com/en/posts/functional-quadtree-clojure) and while that post did include a demo, it left a big exercise with the reader: Making functional quadtrees with multiple focal points like... Boids!

If you want to understand how exactly the code works, it's explained in [this blogpost](https://lbjgruppen.com/en/posts/TODO)

# Building and running

First [install shadow-cljs](https://shadow-cljs.github.io/docs/UsersGuide.html#_installation).

Then clone this repo and run

``` shell
npm install
shadow-cljs watch app
```

After a bit off compilation, browse to `http://localhost:8020` and you should see a fullscreen Quadtree/flocking simulation.

# License

Free to use/modify/replicate/etc.
