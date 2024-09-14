FROM benz0li/ghc-musl:9.10.1
WORKDIR /root/scrod
COPY . /root/scrod
RUN \
  set -o errexit -o xtrace; \
  cabal update; \
  cabal build --enable-executable-static; \
  cp $( cabal list-bin scrod ) .

FROM scratch
EXPOSE 3000
COPY --from=0 /root/scrod/scrod /
CMD ["/scrod"]
