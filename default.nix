{ mkDerivation, attoparsec, base, blaze-builder, bytestring
, case-insensitive, containers, either, exceptions, http-date
, http-media, http-types, lens, mtl, network-uri, QuickCheck
, semigroups, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
, time, transformers, utf8-string
}:
mkDerivation {
  pname = "webcrank";
  version = "0.2.2";
  src = builtins.filterSource (path: type: baseNameOf path != ".git" && baseNameOf path != "dist") ./.;
  buildDepends = [
    attoparsec base blaze-builder bytestring case-insensitive
    containers either exceptions http-date http-media http-types lens
    mtl network-uri semigroups text transformers utf8-string
  ];
  testDepends = [
    attoparsec base blaze-builder bytestring case-insensitive
    containers either exceptions http-date http-media http-types lens
    mtl network-uri QuickCheck semigroups tasty tasty-hunit
    tasty-quickcheck text time transformers utf8-string
  ];
  homepage = "https://github.com/webcrank/webcrank";
  description = "Webmachine inspired toolkit for building http applications and services";
  license = stdenv.lib.licenses.bsd3;
}
