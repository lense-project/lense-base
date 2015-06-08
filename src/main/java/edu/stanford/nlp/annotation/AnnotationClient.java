package edu.stanford.nlp.annotation;

import edu.stanford.nlp.pipeline.Annotation;

import java.io.*;
import java.net.Socket;
import java.net.UnknownHostException;

/**
 * Hooks itself up to the annotation server, allows real time annotations without the startup cost.
 */
public class AnnotationClient {
    public final static String SERVER_HOSTNAME = "localhost";
    public final static int COMM_PORT = 2109;

    public static Annotation annotate(String str) {
        Annotation annotation = null;

        Socket socket = null;
        try
        {
            socket = new Socket(SERVER_HOSTNAME, COMM_PORT);
            OutputStream oStream = socket.getOutputStream();
            ObjectOutputStream ooStream = new ObjectOutputStream(oStream);
            ooStream.writeObject(str);

            InputStream iStream = socket.getInputStream();
            ObjectInputStream oiStream = new ObjectInputStream(iStream);
            annotation = (Annotation) oiStream.readObject();
            oiStream.close();
        }
        catch (UnknownHostException uhe)
        {
            System.out.println("Don't know about host: " + SERVER_HOSTNAME);
            System.exit(1);
        }
        catch (IOException ioe)
        {
            System.out.println("Couldn't get I/O for the connection to: " +
                    SERVER_HOSTNAME + ":" + COMM_PORT);
            System.exit(1);
        }
        catch(ClassNotFoundException cne)
        {
            System.out.println("Wanted class AnnotationWrapper, but got class " + cne);
        }
        finally {
            if (socket != null) {
                try {
                    socket.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

        return annotation;
    }

    public static void main(String[] args) {
        AnnotationClient.annotate("hello my darlings");
    }
}
