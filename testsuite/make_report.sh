#!/usr/bin/env sh

function output()
{
    echo $@ "\\c"
}

TITLE=$1
shift 1

echo "<h1>${TITLE}</h1>"
echo "<table class=\"test-results\">"

SUCCESS=0
TOTAL=0

for test in $@
do
    TOTAL=$(($TOTAL + 1))
    output "<tr> <th>${test}</th>"

    if [ -e ${test}.ll ];
    then
        SUCCESS=$(($SUCCESS + 1))
        output "<th class=\"ok\">ok</th>"
    else
        output "<th class=\"failed\">failed</th>"
    fi

    output "</th> </tr>"
    echo
done

echo "</table>"

# TODO: figure out why this appears before the table...
echo "<p>${SUCCESS}/${TOTAL} succeeded</p>"

