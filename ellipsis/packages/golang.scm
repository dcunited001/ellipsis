(define-module (ellipsis packages golang)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build go-build-system)
  #:use-module (guix build-system go)
  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module ((guix licenses) #:prefix license:))

;; === AWS ====================
;; go-github-com-aws-sdk
;; go-github-com-aws-smithy-go
;; go-github-com-aws-aws-sdk-go
;; go-github-com-aws-aws-sdk-go-v2
;; go-github-com-aws-aws-sdk-go-v2-config
;; go-github-com-aws-aws-sdk-go-v2-service-s3
;; go-github-com-aws-aws-sdk-go-v2-service-iam
;; go-github-com-aws-aws-sdk-go-v2-service-sso
;; go-github-com-aws-aws-sdk-go-v2-service-sts
;; go-github-com-aws-aws-sdk-go-v2-service-ssooidc
;; go-github-com-aws-aws-sdk-go-v2-feature-s3-manager


;; === X ====================
;; go-golang-org-x-xerrors
;; go-golang-org-x-tools
;; go-golang-org-x-exp
;; go-golang-org-x-mod
;; go-golang-org-x-net
;; go-golang-org-x-sys
;; go-golang-org-x-lint
;; go-golang-org-x-sync
;; go-golang-org-x-term
;; go-golang-org-x-text
;; go-golang-org-x-time
;; go-golang-org-x-vuln
;; go-golang-org-x-image
;; go-golang-org-x-crypto
;; go-golang-org-x-oauth2

(define-public sops
  (package
    (name "sops")
    (version "3.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mozilla/sops")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x4b470w3kdcamgbsn2x962fa60s6ak6pqfv9vc3pybmnp03vbg0"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go.mozilla.org/sops/v3"
       #:unpack-path "go.mozilla.org/sops/v3"))

    (propagated-inputs `(,go-gopkg-in-yaml-v2

                         ;; go-gopkg-in-check-v1 1.0.0-20201130134442-10cb98267c6c
                         ;; ("go-gopkg-in-check-v1-1.0.0-20190902080502-41f04d3bba15" ,go-gopkg-in-check-v1-1.0.0-20190902080502-41f04d3bba15)

                         ;; n/a
                         ;; ("go-google-golang-org-genproto-googleapis-api-0.0.0-20230920204549-e6e6cdab5c13" ,go-google-golang-org-genproto-googleapis-api-0.0.0-20230920204549-e6e6cdab5c13)

                         ;; n/a
                         ;; ("go-google-golang-org-genproto-0.0.0-20231002182017-d307bd883b97" ,go-google-golang-org-genproto-0.0.0-20231002182017-d307bd883b97)

                         ;; n/a
                         ;; ("go-google-golang-org-appengine-1.6.7" ,go-google-golang-org-appengine-1.6.7)

                         ;; === x ====================

                         ;; go-golang-org-x-xerrors
                         ;; 0.0.0-0.5ec99f8
                         ;; ("go-golang-org-x-xerrors-0.0.0-20220907171357-04be3eba64a2" ,go-golang-org-x-xerrors-0.0.0-20220907171357-04be3eba64a2)

                         ;; go-golang-org-x-tools
                         ;; 0.5.0
                         ;; ("go-golang-org-x-tools-0.7.0" ,go-golang-org-x-tools-0.7.0)
                         ;; ("go-golang-org-x-time-0.0.0-20220609170525-579cf78fd858" ,go-golang-org-x-time-0.0.0-20220609170525-579cf78fd858)
                         ;; ("go-golang-org-x-text-0.13.0" ,go-golang-org-x-text-0.13.0)
                         ;; go-golang-org-x-sync
                         ;; 0.1.0-1.8fcdb60
                         ;; ("go-golang-org-x-sync-0.3.0" ,go-golang-org-x-sync-0.3.0)

                         ;; go-golang-org-x-oauth2
                         ;; 0.0.0-1.0f29369
                         ;; ("go-golang-org-x-oauth2-0.12.0" ,go-golang-org-x-oauth2-0.12.0)

                         ;; go-golang-org-x-mod
                         ;; 0.7.0-0.7c05a44
                         ;; ("go-golang-org-x-mod-0.9.0" ,go-golang-org-x-mod-0.9.0)

                         ;; go-golang-org-x-crypto
                         ;; 0.4.0
                         ;; ("go-golang-org-x-crypto-0.14.0" ,go-golang-org-x-crypto-0.14.0)

                         ;; --------------------

                         ;; n/a
                         ;; ("go-go-opencensus-io-0.24.0" ,go-go-opencensus-io-0.24.0)

                         ;; === json schema/pointer/ref ====================

                         ;; go-github-com-xeipuuv-gojsonschema 0.0.0-0.6b67b3f
                         ;; ("go-github-com-xeipuuv-gojsonschema-1.2.0" ,go-github-com-xeipuuv-gojsonschema-1.2.0)

                         ;; go-github-com-xeipuuv-gojsonreference 0.0.0-0.bd5ef7b
                         ;; ("go-github-com-xeipuuv-gojsonreference-0.0.0-20180127040603-bd5ef7bd5415" ,go-github-com-xeipuuv-gojsonreference-0.0.0-20180127040603-bd5ef7bd5415)

                         ;; go-github-com-xeipuuv-gojsonpointer 0.0.0-0.4e3ac27
                         ;; ("go-github-com-xeipuuv-gojsonpointer-0.0.0-20180127040702-4e3ac2762d5f" ,go-github-com-xeipuuv-gojsonpointer-0.0.0-20180127040702-4e3ac2762d5f)

                         ;; --------------------


                         ;; ("go-github-com-ryanuber-go-glob-1.0.0" ,go-github-com-ryanuber-go-glob-1.0.0)
                         ;; ("go-github-com-russross-blackfriday-v2-2.1.0" ,go-github-com-russross-blackfriday-v2-2.1.0)
                         ;; ("go-github-com-pmezard-go-difflib-1.0.0" ,go-github-com-pmezard-go-difflib-1.0.0)
                         ;; ("go-github-com-pkg-browser-0.0.0-20210911075715-681adbf594b8" ,go-github-com-pkg-browser-0.0.0-20210911075715-681adbf594b8)
                         ;; ("go-github-com-opencontainers-runc-1.1.5" ,go-github-com-opencontainers-runc-1.1.5)
                         ;; ("go-github-com-opencontainers-image-spec-1.0.2" ,go-github-com-opencontainers-image-spec-1.0.2)
                         ;; ("go-github-com-opencontainers-go-digest-1.0.0" ,go-github-com-opencontainers-go-digest-1.0.0)
                         ;; ("go-github-com-moby-term-0.0.0-20201216013528-df9cb8a40635" ,go-github-com-moby-term-0.0.0-20201216013528-df9cb8a40635)
                         ;; ("go-github-com-mitchellh-mapstructure-1.5.0" ,go-github-com-mitchellh-mapstructure-1.5.0)
                         ;; ("go-github-com-mattn-go-isatty-0.0.17" ,go-github-com-mattn-go-isatty-0.0.17)
                         ;; ("go-github-com-mattn-go-colorable-0.1.13" ,go-github-com-mattn-go-colorable-0.1.13)
                         ;; ("go-github-com-kylelemons-godebug-1.1.0" ,go-github-com-kylelemons-godebug-1.1.0)
                         ;; ("go-github-com-kr-text-0.2.0" ,go-github-com-kr-text-0.2.0)
                         ;; ("go-github-com-jmespath-go-jmespath-0.4.0" ,go-github-com-jmespath-go-jmespath-0.4.0)
                         ;; ("go-github-com-imdario-mergo-0.3.12" ,go-github-com-imdario-mergo-0.3.12)

                         ;; === hashicorp ====================

                         ;; ("go-github-com-hashicorp-hcl-1.0.0" ,go-github-com-hashicorp-hcl-1.0.0)
                         ;; ("go-github-com-hashicorp-go-sockaddr-1.0.2" ,go-github-com-hashicorp-go-sockaddr-1.0.2)
                         ;; ("go-github-com-hashicorp-go-secure-stdlib-strutil-0.1.2" ,go-github-com-hashicorp-go-secure-stdlib-strutil-0.1.2)
                         ;; ("go-github-com-hashicorp-go-secure-stdlib-parseutil-0.1.6" ,go-github-com-hashicorp-go-secure-stdlib-parseutil-0.1.6)
                         ;; ("go-github-com-hashicorp-go-rootcerts-1.0.2" ,go-github-com-hashicorp-go-rootcerts-1.0.2)
                         ;; ("go-github-com-hashicorp-go-retryablehttp-0.7.1" ,go-github-com-hashicorp-go-retryablehttp-0.7.1)
                         ;; ("go-github-com-hashicorp-go-multierror-1.1.1" ,go-github-com-hashicorp-go-multierror-1.1.1)
                         ;; ("go-github-com-hashicorp-go-hclog-1.2.1" ,go-github-com-hashicorp-go-hclog-1.2.1)
                         ;; ("go-github-com-hashicorp-errwrap-1.1.0" ,go-github-com-hashicorp-errwrap-1.1.0)

                         ;; === google ====================

                         ;; ("go-github-com-googleapis-gax-go-v2-2.12.0" ,go-github-com-googleapis-gax-go-v2-2.12.0)
                         ;; ("go-github-com-googleapis-enterprise-certificate-proxy-0.3.1" ,go-github-com-googleapis-enterprise-certificate-proxy-0.3.1)
                         ;; ("go-github-com-google-uuid-1.3.1" ,go-github-com-google-uuid-1.3.1)
                         ;; ("go-github-com-google-s2a-go-0.1.7" ,go-github-com-google-s2a-go-0.1.7)
                         ;; ("go-github-com-golang-groupcache-0.0.0-20210331224755-41bb18bfe9da" ,go-github-com-golang-groupcache-0.0.0-20210331224755-41bb18bfe9da)
                         ;; ("go-github-com-golang-jwt-jwt-v5-5.0.0" ,go-github-com-golang-jwt-jwt-v5-5.0.0)
                         ;; ("go-github-com-gogo-protobuf-1.3.2" ,go-github-com-gogo-protobuf-1.3.2)
                         ;; ("go-github-com-go-jose-go-jose-v3-3.0.0" ,go-github-com-go-jose-go-jose-v3-3.0.0)

                         ;; === docker ====================

                         ;; ("go-github-com-docker-go-units-0.4.0" ,go-github-com-docker-go-units-0.4.0)
                         ;; ("go-github-com-docker-go-connections-0.4.0" ,go-github-com-docker-go-connections-0.4.0)
                         ;; ("go-github-com-docker-docker-20.10.24+incompatible" ,go-github-com-docker-docker-20.10.24+incompatible)
                         ;; ("go-github-com-docker-cli-20.10.17+incompatible" ,go-github-com-docker-cli-20.10.17+incompatible)

                         ;; --------------------

                         ;; ("go-github-com-davecgh-go-spew-1.1.1" ,go-github-com-davecgh-go-spew-1.1.1)
                         ;; ("go-github-com-cpuguy83-go-md2man-v2-2.0.2" ,go-github-com-cpuguy83-go-md2man-v2-2.0.2)
                         ;; ("go-github-com-containerd-continuity-0.3.0" ,go-github-com-containerd-continuity-0.3.0)
                         ;; ("go-github-com-cloudflare-circl-1.3.3" ,go-github-com-cloudflare-circl-1.3.3)
                         ;; ("go-github-com-cenkalti-backoff-v4-4.1.3" ,go-github-com-cenkalti-backoff-v4-4.1.3)
                         ;; ("go-github-com-cenkalti-backoff-v3-3.2.2" ,go-github-com-cenkalti-backoff-v3-3.2.2)

                         ;; === aws ====================

                         ;; go-github-com-aws-smithy-go
                         ;; 1.13.5
                         ;; ("go-github-com-aws-smithy-go-1.15.0" ,go-github-com-aws-smithy-go-1.15.0)

                         ;; go-github-com-aws-aws-sdk-go-v2-service-ssooidc
                         ;; 1.13.10
                         ;; ("go-github-com-aws-aws-sdk-go-v2-service-ssooidc-1.17.2" ,go-github-com-aws-aws-sdk-go-v2-service-ssooidc-1.17.2)

                         ;; go-github-com-aws-aws-sdk-go-v2-service-sso
                         ;; 1.11.27
                         ;; ("go-github-com-aws-aws-sdk-go-v2-service-sso-1.15.1" ,go-github-com-aws-aws-sdk-go-v2-service-sso-1.15.1)

                         ;; n/a
                         ;; ("go-github-com-aws-aws-sdk-go-v2-service-internal-s3shared-1.15.5" ,go-github-com-aws-aws-sdk-go-v2-service-internal-s3shared-1.15.5)

                         ;; n/a
                         ;; ("go-github-com-aws-aws-sdk-go-v2-service-internal-presigned-url-1.9.36" ,go-github-com-aws-aws-sdk-go-v2-service-internal-presigned-url-1.9.36)

                         ;; n/a
                         ;; ("go-github-com-aws-aws-sdk-go-v2-service-internal-checksum-1.1.37" ,go-github-com-aws-aws-sdk-go-v2-service-internal-checksum-1.1.37)

                         ;; n/a
                         ;; ("go-github-com-aws-aws-sdk-go-v2-service-internal-accept-encoding-1.9.15" ,go-github-com-aws-aws-sdk-go-v2-service-internal-accept-encoding-1.9.15)

                         ;; n/a
                         ;; ("go-github-com-aws-aws-sdk-go-v2-internal-v4a-1.1.5" ,go-github-com-aws-aws-sdk-go-v2-internal-v4a-1.1.5)

                         ;; n/a
                         ;; ("go-github-com-aws-aws-sdk-go-v2-internal-ini-1.3.44" ,go-github-com-aws-aws-sdk-go-v2-internal-ini-1.3.44)

                         ;; n/a
                         ;; ("go-github-com-aws-aws-sdk-go-v2-internal-endpoints-v2-2.4.36" ,go-github-com-aws-aws-sdk-go-v2-internal-endpoints-v2-2.4.36)
                         ;; ("go-github-com-aws-aws-sdk-go-v2-internal-configsources-1.1.42" ,go-github-com-aws-aws-sdk-go-v2-internal-configsources-1.1.42)
                         ;; ("go-github-com-aws-aws-sdk-go-v2-feature-ec2-imds-1.13.12" ,go-github-com-aws-aws-sdk-go-v2-feature-ec2-imds-1.13.12)
                         ;; ("go-github-com-aws-aws-sdk-go-v2-aws-protocol-eventstream-1.4.14" ,go-github-com-aws-aws-sdk-go-v2-aws-protocol-eventstream-1.4.14)

                         ;; --------------------

                         ;; ("go-github-com-nvveen-gotty-0.0.0-20120604004816-cd527374f1e5" ,go-github-com-nvveen-gotty-0.0.0-20120604004816-cd527374f1e5)
                         ;; ("go-github-com-microsoft-go-winio-0.6.0" ,go-github-com-microsoft-go-winio-0.6.0)

                         ;; === azure ====================

                         ;; ("go-github-com-azuread-microsoft-authentication-library-for-go-1.1.1" ,go-github-com-azuread-microsoft-authentication-library-for-go-1.1.1)
                         ;; ("go-github-com-azure-go-ansiterm-0.0.0-20170929234023-d6e3b3328b78" ,go-github-com-azure-go-ansiterm-0.0.0-20170929234023-d6e3b3328b78)
                         ;; ("go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-internal-1.0.0" ,go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-internal-1.0.0)
                         ;; ("go-github-com-azure-azure-sdk-for-go-sdk-internal-1.3.0" ,go-github-com-azure-azure-sdk-for-go-sdk-internal-1.3.0)

                         ;; === cloud.google.com ====================

                         ;; ("go-cloud-google-com-go-iam-1.1.2" ,go-cloud-google-com-go-iam-1.1.2)

                         ;; go-cloud-google-com-go-compute-metadata
                         ;; 0.81.0
                         ;; ("go-cloud-google-com-go-compute-metadata-0.2.3" ,go-cloud-google-com-go-compute-metadata-0.2.3)

                         ;; n/a
                         ;; ("go-cloud-google-com-go-compute-1.23.0" ,go-cloud-google-com-go-compute-1.23.0)
                         ;; ("go-cloud-google-com-go-0.110.8" ,go-cloud-google-com-go-0.110.8)

                         ;; === yaml/ini ====================

                         ,go-gopkg-in-yaml-v3
                         ;; 3.0.1
                         ;; ("go-gopkg-in-yaml-v3-3.0.1" ,go-gopkg-in-yaml-v3-3.0.1)

                         ;; go-gopkg-in-ini-v1
                         ;; 1.56.0
                         ;; ("go-gopkg-in-ini-v1-1.67.0" ,go-gopkg-in-ini-v1-1.67.0)

                         ;; === google.golang.org ====================

                         ;; go-google-golang-org-protobuf
                         ;; 1.28.0
                         ;; ("go-google-golang-org-protobuf-1.31.0" ,go-google-golang-org-protobuf-1.31.0)

                         ;; n/a
                         ;; ("go-google-golang-org-grpc-1.58.3" ,go-google-golang-org-grpc-1.58.3)

                         ;; n/a
                         ;; ("go-google-golang-org-genproto-googleapis-rpc-0.0.0-20231009173412-8bfb1ae86b6c" ,go-google-golang-org-genproto-googleapis-rpc-0.0.0-20231009173412-8bfb1ae86b6c)

                         ;; n/a
                         ;; ("go-google-golang-org-api-0.146.0" ,go-google-golang-org-api-0.146.0)

                         ;; === X (again) ====================

                         ;; go-golang-org-x-term
                         ;; 0.3.0
                         ;; ("go-golang-org-x-term-0.13.0" ,go-golang-org-x-term-0.13.0)

                         ;; go-golang-org-x-sys
                         ;; 0.4.0-0.b60007c
                         ;; ("go-golang-org-x-sys-0.13.0" ,go-golang-org-x-sys-0.13.0)

                         ;; go-golang-org-x-net
                         ;; 0.5.0-0.8e0e7d8
                         ;; ("go-golang-org-x-net-0.17.0" ,go-golang-org-x-net-0.17.0)

                         ;; --------------------

                         ,go-github-com-urfave-cli
                         ;; 1.22.2
                         ;; ("go-github-com-urfave-cli-1.22.14" ,go-github-com-urfave-cli-1.22.14)

                         ;; go-github-com-stretchr-testify
                         ;; 1.7.0
                         ;; ("go-github-com-stretchr-testify-1.8.4" ,go-github-com-stretchr-testify-1.8.4)

                         ;; go-github-com-sirupsen-logrus
                         ;; 1.9.0
                         ;; ("go-github-com-sirupsen-logrus-1.9.3" ,go-github-com-sirupsen-logrus-1.9.3)

                         ,go-github-com-pkg-errors
                         ;; 0.9.1
                         ;; ("go-github-com-pkg-errors-0.9.1" ,go-github-com-pkg-errors-0.9.1)

                         ;; n/a
                         ;; ("go-github-com-ory-dockertest-v3-3.10.0" ,go-github-com-ory-dockertest-v3-3.10.0)

                         ,go-github-com-mitchellh-go-wordwrap
                         ;; 1.0.1
                         ;; ("go-github-com-mitchellh-go-wordwrap-1.0.1" ,go-github-com-mitchellh-go-wordwrap-1.0.1)

                         ;; go-github-com-mitchellh-go-homedir
                         ;; 1.0.0-0.ae18d6b
                         ;; ("go-github-com-mitchellh-go-homedir-1.1.0" ,go-github-com-mitchellh-go-homedir-1.1.0)

                         ;; go-github-com-lib-pq
                         ;; 1.2.0
                         ;; ("go-github-com-lib-pq-1.10.9" ,go-github-com-lib-pq-1.10.9)

                         ;; === Hashicorp (again) ====================

                         ;; ("go-github-com-hashicorp-vault-api-1.10.0" ,go-github-com-hashicorp-vault-api-1.10.0)
                         ;; ("go-github-com-hashicorp-go-cleanhttp-0.5.2" ,go-github-com-hashicorp-go-cleanhttp-0.5.2)

                         ;; --------------------

                         ;; ("go-github-com-goware-prefixer-0.0.0-20160118172347-395022866408" ,go-github-com-goware-prefixer-0.0.0-20160118172347-395022866408)
                         ;; go-github-com-google-shlex
                         ;; 0.0.0-20191202100458-e7afc7fbc510
                         ;; ("go-github-com-google-shlex-0.0.0-20191202100458-e7afc7fbc510" ,go-github-com-google-shlex-0.0.0-20191202100458-e7afc7fbc510)

                         ;; n/a
                         ;; ("go-github-com-google-go-cmp-0.6.0" ,go-github-com-google-go-cmp-0.6.0)

                         ;; n/a
                         ;; ("go-github-com-golang-protobuf-1.5.3" ,go-github-com-golang-protobuf-1.5.3)
                         ;; ("go-github-com-getsops-gopgagent-0.0.0-20170926210634-4d7ea76ff71a" ,go-github-com-getsops-gopgagent-0.0.0-20170926210634-4d7ea76ff71a)
                         ;; ("go-github-com-fatih-color-1.15.0" ,go-github-com-fatih-color-1.15.0)
                         ;; ("go-github-com-blang-semver-3.5.1+incompatible" ,go-github-com-blang-semver-3.5.1+incompatible)

                         ;; === AWS (again) ====================

                         ;; go-github-com-aws-aws-sdk-go-v2-service-sts 1.17.7
                         ;; ("go-github-com-aws-aws-sdk-go-v2-service-sts-1.23.1" ,go-github-com-aws-aws-sdk-go-v2-service-sts-1.23.1)

                         ;; go-github-com-aws-aws-sdk-go-v2-service-s3
                         ;; 1.30.0
                         ;; ("go-github-com-aws-aws-sdk-go-v2-service-s3-1.40.1" ,go-github-com-aws-aws-sdk-go-v2-service-s3-1.40.1)

                         ;; n/a
                         ;; ("go-github-com-aws-aws-sdk-go-v2-service-kms-1.24.6" ,go-github-com-aws-aws-sdk-go-v2-service-kms-1.24.6)

                         ;; n/a
                         ;; ("go-github-com-aws-aws-sdk-go-v2-feature-s3-manager-1.11.89" ,go-github-com-aws-aws-sdk-go-v2-feature-s3-manager-1.11.89)

                         ;; n/a
                         ;; ("go-github-com-aws-aws-sdk-go-v2-credentials-1.13.42" ,go-github-com-aws-aws-sdk-go-v2-credentials-1.13.42)

                         ;; go-github-com-aws-aws-sdk-go-v2-config
                         ;; 1.18.5
                         ;; ("go-github-com-aws-aws-sdk-go-v2-config-1.18.44" ,go-github-com-aws-aws-sdk-go-v2-config-1.18.44)

                         ;; go-github-com-aws-aws-sdk-go-v2
                         ;; 1.17.3
                         ;; ("go-github-com-aws-aws-sdk-go-v2-1.21.1" ,go-github-com-aws-aws-sdk-go-v2-1.21.1)

                         ;; === Protonmail ====================

                         ;; ("go-github-com-protonmail-go-crypto-0.0.0-20230923063757-afb1ddc0824c" ,go-github-com-protonmail-go-crypto-0.0.0-20230923063757-afb1ddc0824c)

                         ;; === Azure (again) ====================

                         ;; ("go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-azkeys-1.0.1" ,go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-azkeys-1.0.1)
                         ;; ("go-github-com-azure-azure-sdk-for-go-sdk-azidentity-1.4.0" ,go-github-com-azure-azure-sdk-for-go-sdk-azidentity-1.4.0)
                         ;; ("go-github-com-azure-azure-sdk-for-go-sdk-azcore-1.8.0" ,go-github-com-azure-azure-sdk-for-go-sdk-azcore-1.8.0)

                         ;; --------------------

                         ;; ("go-filippo-io-age-1.1.1" ,go-filippo-io-age-1.1.1)
                         ;; ("go-cloud-google-com-go-storage-1.33.0" ,go-cloud-google-com-go-storage-1.33.0)
                         ;; ("go-cloud-google-com-go-kms-1.15.2" ,go-cloud-google-com-go-kms-1.15.2)
                         ))
    (home-page "https://go.mozilla.org/sops/v3")
    (synopsis #f)
    (description
     "Package sops manages JSON, YAML and BINARY documents to be encrypted or
decrypted.")
    (license license:mpl2.0)))
