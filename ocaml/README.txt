Description of files:
tipi.ml : contains types used to describe TSB processes and UPPAAL automa 
mapping.ml : contains functions to convert a TSB process into an automa. 
             Main functions are located at the end of the file.
toXML.ml: contains functions to convert and UPPAAL automa into xml, 
          and also to write an xml to file.
test.ml: contains examples 

Examples: 
1)To run an example, first of all import the file toXML, which recursively, 
will import both mapping and tipi. 
e.g:
#use "toXML.ml";;

2)Then write the TSB process
e.g:
let p = IntChoice [(TSBAction "a", TSBGuard [(TSBClock "t", Less, 10)], TSBReset[] , Success)];;

3) Convert it into an automa, providing an identifier
e.g:
let aut = buildAutomaMain p "p";;

4) Convert the automa in a xml message and write it down in a file.
Function writeToFile needs a list of automata, so enclose the automa in square brackets.
The provided filename must not contain any extension.
e.g:
writeToFile [aut] "ex1"

5) When mapping two processes at once, you can use the function tsb_mapping
which returns a list of automata:
e.g:
let lta = tsb_mapping p q;;
writeToFile lta "ex50";;
