;;; Tarot --- Simple handler of Tarot cards

(use-modules (guix)
             (guix build-system haskell)
             (guix channels)
             (guix git-download)
             (guix inferior)
             ((guix licenses) #:prefix license:)
             (guix profiles)
             (gnu packages)
             (gnu packages haskell-xyz))

;; Borrowed from the Guix Cookbook itself to enable building locally from repo
;; source: https://guix.gnu.org/cookbook/en/html_node/Getting-Started.html
(define vcs-file?
  ;; Return true if the given file is under version control.
  (or (git-predicate (current-source-directory))
      (const #t)))  ; not in a Git checkout

;; === Inferior setup ===
(define haskell-inferior
  (let ((channels (list (channel
                         (name 'guix)
                         (url "https://codeberg.org/guix/guix.git")
                         (branch "haskell-team")
                         (commit "bf5516c3fd668fa33b0cf73639a192554b5fb267")
                         (introduction
                          (make-channel-introduction
                           "1edfe6dd7b62ac2c9f3648ee1379588de45f3e74"
                           (openpgp-fingerprint
                            "3CE4 6455 8A84 FDC6 9DB4  0CFB 090B 1199 3D9A EBB5")))))))
    (inferior-for-channels channels)))

(define-syntax-rule (inferior-haskell-package name version)
  (car (lookup-inferior-packages haskell-inferior name version)))

;; === Dependencies ===
(define ghc-9.10
  (inferior-haskell-package "ghc" "9.10.2"))

(define cabal-install
  (inferior-haskell-package "cabal-install" "3.12.1.0"))

(define-public ghc-random-1.3.1
  (package
    (inherit ghc-random)
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "random" version))
       (sha256
        (base32 "0d8snwlrq8x4r197q1igpvwhrdbyc9wfry3qlsiczc35ya1sqh6q"))))
    (inputs (list ghc-splitmix ghc-data-array-byte))))

;; === Package Specification ===

(define-public tarot-hs
  (package
    (name "tarot-hs")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jbelldev/tarot-hs")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qbbxaqja10mkrgq4p4ml7asyrsdb0jy2gpbfvwjhzygf9m608ri"))))
    (build-system haskell-build-system)
    (arguments `(#:haskell ,ghc-9.10))
    (inputs (list ghc-random-1.3.1))
    (native-inputs (list ghc-9.10
                         cabal-install))
    (synopsis "A simple project for exploring Literate Programming.")
    (description "A simple project for exploring Literate Programming.")
    (home-page "https://github.com/jbelldev/tarot-hs")
    (license license:gpl3+)))

tarot-hs
