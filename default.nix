{ mkDerivation, attoparsec, base, bytestring, case-insensitive
, http-date, http-media, http-types, mtl, network-uri, QuickCheck
, semigroups, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
, time, transformers, utf8-string
}:
mkDerivation {
  pname = "webcrank";
  version = "0.0.1";
  src = ./.;
  buildDepends = [
    attoparsec base bytestring case-insensitive http-date http-media
    http-types mtl network-uri semigroups text time transformers
    utf8-string
  ];
  testDepends = [
    attoparsec base bytestring case-insensitive http-types mtl
    QuickCheck semigroups tasty tasty-hunit tasty-quickcheck text
    transformers
  ];
  homepage = "https://github.com/webcrank/webcrank";
  description = "Webmachine inspired toolkit for building http applications and services";
  license = stdenv.lib.licenses.bsd3;
}
