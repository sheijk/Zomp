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
        output "<th> <p class=\"missing-file\" title=\"File $1 does not exist\">$2</p></th>"
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
    output "<tr>"
    outputLinkIfExists "${test}.zomp" "${test}"
    # output "<tr> <th>${test}</th>"

    if [ -e ${test}.result ];
    then
        grep failed ${test}.result > /dev/null
        if [ "$?" == "0" ];
        then
            output "<th class=\"failed\">`cat ${test}.result`</th>"
        else
            SUCCESS=$(($SUCCESS + 1))
            output "<th class=\"ok\">`cat ${test}.result`</th>"
        fi
    else
        output "<th class=\"failed\">not run</th>"
    fi

    outputLinkIfExists "${test}.testreport" "report"
    outputLinkIfExists "${test}.test_output" "output"

    output "</tr>"
    echo
done

echo "</table>"

echo "<p>${SUCCESS}/${TOTAL} succeeded</p>"

