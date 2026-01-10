;;; Tarot --- Simple handler of Tarot cards

(use-modules (guix)
             (guix build-system haskell)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (gnu packages haskell-apps)
             (gnu packages haskell-xyz))

;; Borrowed from the Guix Cookbook itself to enable building locally from repo
;; source: https://guix.gnu.org/cookbook/en/html_node/Getting-Started.html
(define vcs-file?
  ;; Return true if the given file is under version control.
  (or (git-predicate (current-source-directory))
      (const #t)))  ; not in a Git checkout

(define-public cabal-install-3.12.0
  (package
    (inherit cabal-install)
    (version "3.12.0")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "cabal-install" version))
              (sha256
               (base32
                "0dihpm4h3xh13vnpvwflnb7v614qdvljycc6ffg5cvhwbwfrxyfw"))))))

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

(package
  (name "tarot-hs")
  (version "1.0.0")
  (source #f)
  (build-system haskell-build-system)
  (native-inputs (list ghc-random-1.3.1
                       cabal-install-3.12.0))
  (synopsis "A simple project for exploring Literate Programming.")
  (description "A simple project for exploring Literate Programming.")
  (home-page "https://github.com/jbelldev/tarot-hs")
  (license license:gpl3+))
