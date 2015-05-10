package edu.stanford.nlp.word2vec;

import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;

import java.io.*;
import java.util.HashMap;
import java.util.Map;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

/**
 * Created by keenon on 1/1/15.
 *
 * Loads a big inflated Word2Vec index into memory, where we can use it to populate double[] features for experimentation
 * What we want here is a blocking map, and a loader thread, similar to the TransferMap, but without deleting entries.
 */
public class Word2VecLoader {
    static final int WORD_SIZE = 50;

    public static void main(String[] args) throws IOException {
        translate("data/google-300.txt", "data/google-300.ser.gz");
    }

    // This is an unfortunate necessity, since it seems that Java's InputStream handles file differently than fscanf in
    // C.
    public static void translate(String textPath, String outPath) throws IOException {
        File f = new File(textPath);
        assert(f.exists());

        File o = new File(outPath);
        Output output = new Output(new GZIPOutputStream(new FileOutputStream(o)));

        BufferedReader br = new BufferedReader(new FileReader(f));
        String line = br.readLine(); // ignore first line
        String[] initialParts = line.split(" ");
        int words = Integer.parseInt(initialParts[0]);
        int dimension = Integer.parseInt(initialParts[1]);

        output.writeInt(words);
        output.writeInt(dimension);
        int count = 0;
        while ((line = br.readLine()) != null) {
            String[] parts = line.split(" ");
            output.writeString(parts[0]);
            count++;
            if (count % 100 == 0) {
                System.out.println(((double)count / words)+": "+count+" / "+words);
            }
            for (int i = 0; i < dimension; i++) {
                output.writeFloat(Float.parseFloat(parts[i+1]));
            }
        }

        output.close();
        br.close();
    }

    public static void dump(String outPath, Map<String,double[]> map) throws IOException {
        File o = new File(outPath);
        if (o.exists()) o.delete();
        Output output = new Output(new GZIPOutputStream(new FileOutputStream(o)));

        int dimension = map.values().iterator().next().length;

        int count = 0;
        int words = map.size();

        output.writeInt(words);
        output.writeInt(dimension);
        for (String s : map.keySet()) {
            count++;
            if (count % 100 == 0) {
                System.out.println(((double)count / words)+": "+count+" / "+words);
            }
            output.writeString(s);
            double[] parts = map.get(s);
            for (int i = 0; i < dimension; i++) {
                output.writeFloat((float)parts[i]);
            }
        }

        output.close();
    }

    public static Map<String,double[]> loadData(String path) throws IOException {
        File f = new File(path);
        assert(f.exists());
        Input input;
        if (path.endsWith(".gz")) {
            input = new Input(new GZIPInputStream(new FileInputStream(f)));
        }
        else {
            input = new Input(new FileInputStream(f));
        }

        Map<String,double[]> embeddings = new HashMap<>();
        int words = input.readInt();
        int dimension = input.readInt();

        for (int i = 0; i < words; i++) {
            String s = input.readString();
            double[] arr = new double[dimension];
            for (int j = 0; j < dimension; j++) {
                arr[j] = input.readFloat();
            }
            embeddings.put(s, arr);
        }

        input.close();

        return embeddings;
    }
}
