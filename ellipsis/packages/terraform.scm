(define-module (ellipsis packages terraform)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)

  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)

  #:use-module (srfi srfi-1))

;; NOTE may require ~/.config customization, esp for plugins
(define-public packer-bin
  (package
    (name "packer-bin")
    (version "1.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://releases.hashicorp.com/packer/"
                                  version "/packer_"
                                  version "_linux_amd64.zip"))
              (sha256
               (base32 "0xbjjkknv6bvgh6j8dyfjf0d1sbwvf0vb8yq2npp15prsp84izil"))))
    (build-system copy-build-system)
    (inputs (list unzip))
    (arguments
     '(#:install-plan '(("packer" "bin/"))))
    (home-page "https://www.hashicorp.com/products/packer")
    (synopsis "Packer standardizes and automates the process of building
images")
    (description "Packer is a tool for creating identical machine images for
multiple platforms from a single source configuration.")
    (license license:mpl2.0)))

(define-public terraform-bin
  (package
    (name "terraform-bin")
    (version "1.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://releases.hashicorp.com/terraform/"
                                  version "/terraform_"
                                  version "_linux_amd64.zip"))
              (sha256
               (base32 "1skps2scfjl2d3iqxj7j76rkrry0wyllr8fgm0kz9xzc6x8w1n8n"))))
    (build-system copy-build-system)
    (inputs (list unzip))
    (arguments
     '(#:install-plan '(("terraform" "bin/"))))
    (home-page "https://www.hashicorp.com/products/terraform")
    (synopsis "Infrastructure automation to provision and manage resources in any cloud or
data center")
    (description "Terraform enables you to safely and predictably create, change, and improve
infrastructure. It is an open source tool that codifies APIs into declarative
configuration files that can be shared amongst team members, treated as code,
edited, reviewed, and versioned.")
    (license license:mpl2.0)))
