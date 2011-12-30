#!/usr/bin/env sh

function output()
{
    echo $@ "\\c"
}

TITLE=$1
shift 1

echo "<h1>${TITLE}</h1>"
echo "<table class=\"test-results\">"

for test in $@
do
    output "<tr onMouseOver=\"this.className='highlight'\" onMouseOut=\"this.className='normal'\"> <th>${test}</th>"

    if [ -e ${test}.ll ];
    then
        output "<th class=\"ok\">ok</th>"
    else
        output "<th class=\"failed\">failed</th>"
    fi

    output "</th> </tr>"
    echo
done

echo "</table>"

