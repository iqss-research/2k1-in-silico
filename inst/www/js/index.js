const customHref = function(link){

        // find all links
        const links = document.getElementsByTagName("a");

        // since it returns an object, iterate over each entries
        Object.entries(links).forEach( (elem, i) => {

                // match data-value attribute with input var
                if(elem[1].getAttribute("data-value") === link){

                        // if match, click link
                        elem[1].click()
                }
        });
}