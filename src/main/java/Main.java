import net.sf.javaml.classification.Classifier;
import net.sf.javaml.classification.KNearestNeighbors;
import net.sf.javaml.core.Dataset;
import net.sf.javaml.core.DefaultDataset;
import net.sf.javaml.core.DenseInstance;
import net.sf.javaml.core.Instance;
import net.sf.javaml.tools.InstanceTools;
import net.sf.javaml.tools.data.FileHandler;

import java.io.*;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Created with IntelliJ IDEA.
 * User: deathnik
 * Date: 9/22/13
 * Time: 2:13 AM
 * To change this template use File | Settings | File Templates.
 */
public class Main {
    public static void main(String args[]) throws IOException {
        BufferedReader bf = new BufferedReader(new FileReader(new File("../train.tsv")));
        String input;
        Dataset trainData = new DefaultDataset();
        HashMap<String,Dataset> categories = new HashMap<String, Dataset>();
        while ((input = bf.readLine()) != null) {
            String[] arr = input.split("\t");
            double[] values = new double[23];
            for (int i = 4; i < arr.length - 1; i++) {
                if (arr[i].equals("\"?\"")) {
                    values[i-4] = -10;

                } else {
                    values[i-4] = Double.parseDouble(arr[i].substring(1, arr[i].length() - 1));
                }
            }
            Instance inst = new DenseInstance(values, Integer.parseInt(arr[26].substring(1, arr[26].length() - 1)));

            if(categories.get(arr[3])==null){
                categories.put(arr[3],new DefaultDataset()) ;
            }
            categories.get(arr[3]).add(inst);
        }
        bf.close();
        System.out.println("Loaded " + trainData.size() + " dataset entries");
        HashMap<String,Classifier> knns = new HashMap<String, Classifier>(categories.size());
        Classifier knn;
        for(Map.Entry<String, Dataset> d : categories.entrySet()){
            knn= new KNearestNeighbors(25);
            knn.buildClassifier(d.getValue());
            knns.put(d.getKey(),knn);
        }
        bf = new BufferedReader(new FileReader(new File("../test.tsv")));
        BufferedWriter br = new BufferedWriter(new FileWriter(new File("../out.csv")));
        br.write("urlid,label\n");
        input = bf.readLine();

        while ((input = bf.readLine()) != null) {
            String[] arr = input.split("\t");
            double[] values = new double[23];
            for (int i = 4; i < arr.length - 1; i++) {
                if (arr[i].equals("\"?\"")) {
                    values[i-4] = -10;

                } else {
                    values[i-4] = Double.parseDouble(arr[i].substring(1, arr[i].length() - 1));
                }
            }
            Instance inst = new DenseInstance(values);
            br.write(arr[1].substring(1,arr[1].length()-1)+","+knns.get(arr[3]).classify(inst)+"\n");
        }
        bf.close();
        br.close();

    }
}
