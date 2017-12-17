# Edit this for your own project dependencies
OPAM_DEPENDS="ocamlfind ounit re"
	 
case "$OCAML_VERSION,$OPAM_VERSION" in
4.06.0,1.2.2) ppa=avsm/ocaml45+opam12 ;;
4.05.0,1.2.2) ppa=avsm/ocaml45+opam12 ;;
4.04.0,1.2.2) ppa=avsm/ocaml45+opam12 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac
	 
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1
opam init 
opam install ${OPAM_DEPENDS}
eval `opam config env`
make
