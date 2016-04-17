if which ghc > /dev/null
then
    function h { ghc -e "interact ($*)" Ust.hs ; }
    function hl { h "bylines ($*)" ; }
    function hw { h "bywords ($*)" ; }
fi
