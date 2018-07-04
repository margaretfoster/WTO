import org.jsoup.*;
import org.jsoup.nodes.*;
import org.jsoup.select.*;
import java.io.*;
import java.util.*;
public class TdAHref {
    static boolean recursiveAHref(Element c, Element ns) {
        for (Element a: ns.children()) {
            if (a.tagName().equals("a")) {
                String href = a.attr("href");
                if (href != null &&
                    href.contains("MeetingId=")) {
		    int position =  href.indexOf("MeetingId=");
		    String temp = href.substring(position+10);
		    int end = temp.indexOf("&");
                    System.out.println(c.text() + " : " + temp.substring(0,end));
                    return true;
                }
            }
            if(recursiveAHref(c,a)){
                return true;
            }
            
        }
        
        return false;
    }
    static void findTdAHref(Element current, Element lastTd) {
        for (Element c: current.children()) {
            if (c.tagName().equals("td") ) {
                Element ns = c.nextElementSibling();
                //                System.out.println(c);
                //System.out.println(ns);
                if (ns != null && ns.tagName().equals("td")) {
                    recursiveAHref(c,ns);
                }
            }
            
            findTdAHref(c, lastTd);

        }
    }
    
    public static void main(String [] args) throws Exception {
	for (int i = 0; i < args.length; i++) {
	    Document d = Jsoup.parse(new File(args[i]),null);
            findTdAHref(d.body(), null);
        }
    }
}
