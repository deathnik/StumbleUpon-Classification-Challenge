import java.io.*;
import java.util.*;


public class Main {
    static public Random random = new Random();

    public static void main(String args[]) throws IOException {
        boolean isLabeled = true;
        ArrayList<String[]> data = Main.loadDataFromFile("../train.tsv");
        ArrayList<String[]> train = new ArrayList<String[]>();
        ArrayList<String[]> cv = new ArrayList<String[]>();
        Main.splitData(data, train, cv, 0.0);
        Classifier classifier = new Classifier(train);
        int rightPred = 0;
        cv=train;
        for(int i =0 ; i < cv.size(); i++){
            String arr[] = cv.get(i);
            Integer label = Integer.parseInt(arr[26].substring(1, arr[26].length() - 1));
            if(label ==  classifier.classify(arr)){
                rightPred ++;
            }
        }
        System.out.println(1.0*rightPred/cv.size());

        ArrayList<String[]> test = Main.loadDataFromFile("../test.tsv");
        BufferedWriter br = new BufferedWriter(new FileWriter(new File("../out.csv")));
        br.write("urlid,label\n");
        for(String[] arr : test){
            br.write(arr[1].substring(1, arr[1].length() - 1) + "," + classifier.classify(arr) + "\n");
        }
        br.close();
       /* BufferedReader bf = new BufferedReader(new FileReader(new File("../train.tsv")));
        String input = bf.readLine();
        Dataset trainData = new DefaultDataset();
        HashMap<String, Dataset> categories = new HashMap<String, Dataset>();
        while ((input = bf.readLine()) != null) {
            String[] arr = input.split("\t");
            double[] values = new double[23];
            for (int i = 4; i < arr.length - 2; i++) {
                if (arr[i].equals("\"?\"")) {
                    values[i - 4] = -10;

                } else {
                    values[i - 4] = Double.parseDouble(arr[i].substring(1, arr[i].length() - 1));
                }
            }
            Instance inst = new DenseInstance(values, Integer.parseInt(arr[26].substring(1, arr[26].length() - 1)));

            if (categories.get(arr[3]) == null) {
                categories.put(arr[3], new DefaultDataset());
            }
            categories.get(arr[3]).add(inst);
        }
        bf.close();
        System.out.println("Loaded " + trainData.size() + " dataset entries");
        HashMap<String, Classifier> knns = new HashMap<String, Classifier>(categories.size());
        Classifier knn;
        for (Map.Entry<String, Dataset> d : categories.entrySet()) {
            knn = new KNearestNeighbors(25);
            knn.buildClassifier(d.getValue());
            knns.put(d.getKey(), knn);
        }
        bf = new BufferedReader(new FileReader(new File("../test.tsv")));
        BufferedWriter br = new BufferedWriter(new FileWriter(new File("../out.csv")));
        br.write("urlid,label\n");
        input = bf.readLine();

        while ((input = bf.readLine()) != null) {
            String[] arr = input.split("\t");
            double[] values = new double[23];
            for (int i = 4; i < arr.length - 2; i++) {
                if (arr[i].equals("\"?\"")) {
                    values[i - 4] = -10;

                } else {
                    values[i - 4] = Double.parseDouble(arr[i].substring(1, arr[i].length() - 1));
                }
            }
            Instance inst = new DenseInstance(values);
            br.write(arr[1].substring(1, arr[1].length() - 1) + "," + knns.get(arr[3]).classify(inst) + "\n");
        }
        bf.close();
        br.close();       */

    }

    static public ArrayList<String[]> loadDataFromFile(String file) {
        ArrayList<String[]> result = new ArrayList<String[]>();
        try {
            BufferedReader br = new BufferedReader(new FileReader(new File(file)));
            String input = br.readLine();
            while ((input = br.readLine()) != null) {
                result.add(input.split("\t"));
            }
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return result;
    }

    static public void splitData(ArrayList<String[]> data, ArrayList<String[]> train, ArrayList<String[]> crossValidation, Double part) {
        Set<Integer> s = new HashSet();
        while (s.size() < data.size() * part) {
            s.add(random.nextInt(data.size()));
        }
        int trainIndex = 0;
        int cvIndex = 0;
        for (int i = 0; i < data.size(); i++) {
            if (s.contains(i)) {
                crossValidation.add(cvIndex, data.get(i));
                cvIndex++;
            } else {
                train.add(trainIndex, data.get(i));
                trainIndex++;
            }
        }
    }

}
