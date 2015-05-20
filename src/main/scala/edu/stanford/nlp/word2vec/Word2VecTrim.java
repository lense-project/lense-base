package edu.stanford.nlp.word2vec;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Created by keenon on 1/15/15.
 *
 * This manages removing all words from a dump that aren't part of the dataset, which significantly improves loading
 * times for multiple runs with word embeddings.
 */
public class Word2VecTrim {
    public static void trim(String source, String dest, Set<String> relevant) throws IOException {
        Map<String,double[]> big = Word2VecLoader.loadData(source);
        Map<String,double[]> smaller = new HashMap<>();
        for (String s : relevant) {
            if (big.containsKey(s)) {
                smaller.put(s, big.get(s));
            }
        }
        System.out.println("Reduced from "+big.size()+" to "+smaller.size());
        System.out.println("Thats "+((double)smaller.size() / (double)big.size())+"%!");
        Word2VecLoader.dump(dest, smaller);
    }

    public static void trimToSequenceData(String source, String dest, String[] sequences) throws IOException {
        Set<String> usefulTokens = new HashSet<>();

        for (String sequence : sequences) {
            BufferedReader br = new BufferedReader(new FileReader(sequence));
            String line;
            while ((line = br.readLine()) != null) {
                line = line.replaceAll(" [ \t]*", "\t");
                String[] parts = line.split("\t");
                if (parts.length > 0) {
                    usefulTokens.add(parts[0]);
                }
            }
            br.close();
        }

        trim(source, dest, usefulTokens);
    }

    public static void main(String[] args) throws IOException {
        trimToSequenceData("data/google-300.ser.gz", "realdata/google-300-fulldata.ser.gz", new String[]{
                "realdata/train-seq.txt",
                "realdata/test-seq.txt"
        });
    }
}
