;; Pinned to the latest Guix commit for the Haskell team's branch as of writing.
(list (channel
       (name 'guix)
       (url "https://codeberg.org/guix/guix.git")
       (branch "haskell-team")
       (commit "bf5516c3fd668fa33b0cf73639a192554b5fb267")
       (introduction
        (make-channel-introduction
         "1edfe6dd7b62ac2c9f3648ee1379588de45f3e74"
         (openpgp-fingerprint
          "3CE4 6455 8A84 FDC6 9DB4  0CFB 090B 1199 3D9A EBB5")))))
