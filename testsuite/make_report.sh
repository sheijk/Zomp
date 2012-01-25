#!/usr/bin/env sh

function output()
{
    echo $@ "\\c"
}

function outputLinkIfExists()
{
    if [ -e $1 ]
    then
        output "<th><a href=\"$1\">$2</a></th>"
    else
        output "<th> <p title=\"File $1 does not exist\">$2</p></th>"
    fi
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

    outputLinkIfExists "${test}.testreport" "report"
    outputLinkIfExists "${test}.test_output" "output"

    output "</tr>"
    echo
done

echo "</table>"

# TODO: figure out why this appears before the table...
echo "<p>${SUCCESS}/${TOTAL} succeeded</p>"

