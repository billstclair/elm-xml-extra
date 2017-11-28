This directory contains an example of using the `billstclair/elm-xml-extra` package.

The code is in `example.elm`, which you can run with elm reactor:

    cd .../elm-xml-extra/example
    elm reactor
    
Then aim your web browser at [localhost:8000/example.elm](http://localhost:8000/example.elm). The top of its initial output is below.

**`Decoded:`**

    { name = "noah", age = 50 }

**`XML:`**

    <?xml version="1.0" encoding="UTF-8"?>
    <person>
      <name>noah</name>
      <age max="100">50</age>
    </person>
    <person>
      <name>josh</name>
      <age max="100">57</age>
    </person>

The `Xml.Extra.xmlToJson`, `Xml.xmlToJson`, and `Parsed XML` sections show intermediate results. See the code for details.

There are two checkboxes at the top of the screen. The "complex XML" checkbox, changes the input XML to have a spouse and two children. The "complex Decoder" checkbox changes the decoder to LOOK for spouse and children.
