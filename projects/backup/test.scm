(begin
  (setup 'felipo)
  (create-ring-of-obfuscation 'my-precious (ask me 'location))
  (ask me 'take (thing-named 'my-precious))
  (ask me 'feel-the-force)
  (ask me 'drop (thing-named 'my-precious))
  (ask me 'feel-the-force)
  (newline)
  (define alyssa
    (car (filter (lambda (p) (eq? 'alyssa-hacker (ask p 'name))) (all-people))))
  (define m2 (create-ring-of-obfuscation 'my-precious-2 (ask alyssa 'location)))
  (ask alyssa 'take m2)
  (ask me 'feel-the-force))

(begin
  (setup 'felipo)

  (newline)(display "wand") (newline)
  (create-wand 'my-wand (ask me 'location))
  (ask (thing-named 'my-wand) 'zap me)
  (ask me 'take (thing-named 'my-wand))

  (newline)(display "spell") (newline)
  (clone-spell (pick-random (ask chamber-of-stata 'THINGS)) (ask me 'location))
  (ask me 'look-around)
  (ask me 'take (pick-random (ask me 'stuff-around)))

  (newline)(display "thing") (newline)
  (create-thing 'thing (ask me 'location))

  (newline)(display "zap wand") (newline)
  (ask (thing-named 'my-wand) 'zap me)
  (ask (thing-named 'my-wand) 'zap (thing-named 'thing))
  (ask (thing-named 'my-wand) 'zap (car (all-people)))
  )

(begin
  (setup 'felipo)

  (newline)(display "wand") (newline)
  (create-wand 'my-wand (ask me 'location))
  (ask (thing-named 'my-wand) 'zap me)
  (ask me 'take (thing-named 'my-wand))
  (ask (thing-named 'my-wand) 'zap me)

  (clone-spell (pick-random (ask chamber-of-stata 'THINGS)) (ask me 'location))
  (ask me 'take (pick-random (ask me 'stuff-around)))

  (ask (thing-named 'my-wand) 'zap me)
  (ask (thing-named 'my-wand) 'wave)
  )


(setup 'felipo)
(create-wand 'my-wand (ask me 'location))
(ask me 'take (thing-named 'my-wand))
(clone-spell (car (ask chamber-of-stata 'THINGS)) (ask me 'location))
(ask me 'take (thing-named 'winds-of-doom-spell))
(ask (thing-named 'my-wand) 'zap me)




