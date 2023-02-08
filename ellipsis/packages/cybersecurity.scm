;; (define-public ghidra-jar
;;   (package
;;     (name "ghidra-jar")
;;     (version "10.1.5")
;;     (source (origin
;;               (method url-fetch/zipbomb)
;;               (uri (string-append "https://github.com/NationalSecurityAgency/ghidra" version ".tar.gz"))

;;               (sha256
;;                (base32
;;                 ""))))
;;     (build-system ant-build-system)
;;     (home-page "https://ghidra-sre.org")
;;     (synopsis "Ghidra is a software reverse engineering (SRE) framework.")
;;     (description "Ghidra is a software reverse engineering (SRE) framework created and maintained by the National Security Agency Research Directorate. This framework includes a suite of full-featured, high-end software analysis tools that enable users to analyze compiled code on a variety of platforms including Windows, macOS, and Linux. Capabilities include disassembly, assembly, decompilation, graphing, and scripting, along with hundreds of other features.")
;;     (license license:asl)))
