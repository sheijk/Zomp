
import ocaml
import os

env = Environment(
    ENV = { 'PATH' : os.environ['PATH'] },
    OCAML_CODE='bytecode',       # could be 'bytecode' or 'native'
    OCAML_DEBUG=0,
    OCAML_PROFILE=0
)
env.Tool('ocaml', '.')

env.OcamlProgram('prog', ['ast2.ml', 'genllvm.ml', 'lexer.ml'])

# o = env.OcamlObject('object', 'object.ml')
# l = env.OcamlLibrary('lib', 'lib.ml')
# env.OcamlProgram('prog', 'prog.ml', OCAML_LIBS=l, OCAML_OBJS=o)


