import java.util.*;

public class Classifier {
    HashMap<String, List<Map.Entry<Double, DataSummary>>[]> classifier;

    Classifier(ArrayList<String[]> train) {
        boolean isLabeled = true;
        HashMap<String, HashMap<Double, DataSummary>[]> set = new HashMap<String, HashMap<Double, DataSummary>[]>();
        for (String[] arr : train) {
            if (set.get(arr[3]) == null) {
                HashMap[] h = new HashMap[22];
                for (int i = 0; i < 22; i++) {
                    h[i] = new HashMap<Double, DataSummary>();
                }
                set.put(arr[3], h);
            }
            Integer label = Integer.parseInt(arr[26].substring(1, arr[26].length() - 1));
            HashMap<Double, DataSummary>[] cur = set.get(arr[3]);
            for (int i = 4; i < arr.length - 1; i++) {
                Double val = arr[i].equals("\"?\"") ? -10 : Double.parseDouble(arr[i].substring(1, arr[i].length() - 1));
                if (cur[i - 4].get(val) == null) {
                    cur[i - 4].put(val, new DataSummary(val, label));
                } else {
                    cur[i - 4].get(val).addLabel(label);
                }
            }
        }
        classifier = new HashMap<String, List<Map.Entry<Double, DataSummary>>[]>();
        for (Map.Entry<String, HashMap<Double, DataSummary>[]> category : set.entrySet()) {
            List<Map.Entry<Double, DataSummary>>[] listMas = new List[22];
            EntryComparator ec = new EntryComparator();
            for (int i = 0; i < 22; i++) {
                HashMap<Double, DataSummary>[] hm = category.getValue();
                List<Map.Entry<Double, DataSummary>> ls = new ArrayList<Map.Entry<Double, DataSummary>>();
                ls.addAll(hm[i].entrySet());
                Collections.sort(ls, ec);
                listMas[i] = ls;
            }
            classifier.put(category.getKey(), listMas);
        }
    }

    public Integer classify(String[] arr) {
        double posPrediction = 0.0;
        double negPrediction = 0.0;
        List<Map.Entry<Double, DataSummary>>[] ls = classifier.get(arr[3]);
        for (int i = 4; i < arr.length - 1; i++) {
            Double val = arr[i].equals("\"?\"") ? -10 : Double.parseDouble(arr[i].substring(1, arr[i].length() - 1));
            if (ls[i - 4].get(ls[i - 4].size() - 1).getKey() < val) {
                double prob = ls[i - 4].get(ls[i - 4].size() - 1).getValue().getProbability();
                posPrediction += prob;
                negPrediction += (1.0 - prob);
                continue;
            }
            for (int j = 0; j < ls[i - 4].size(); j++) {
                if (ls[i - 4].get(j).getKey() <= val) {
                    if (j + 1 >= ls[i - 4].size()) {
                        double prob = ls[i - 4].get(j).getValue().getProbability();
                        posPrediction += prob;
                        negPrediction += (1.0 - prob);
                        break;
                    } else {
                        DataSummary ds1 = ls[i - 4].get(j).getValue();
                        DataSummary ds2 = ls[i - 4].get(j + 1).getValue();
                        Double distance = ds2.data - ds1.data;
                        Double prob = ds1.getProbability()*(val  - ds1.data)/distance + ds2.getProbability()*( ds2.data-val)/distance ;
                        posPrediction += prob;
                        negPrediction += (1.0 - prob);
                        break;
                    }
                }
                //two neighbours
              /*  if(ls[i - 4].get(j).getKey() < val){
                    double prob
                }  */
            }
        }

        double mult = 1.0 / (posPrediction + negPrediction);
        if (posPrediction * mult > 0.5) return 1;
        else return 0;
    }


    class EntryComparator implements Comparator<Map.Entry<Double, DataSummary>> {
        @Override
        public int compare(Map.Entry<Double, DataSummary> doubleDataSummaryEntry, Map.Entry<Double, DataSummary> doubleDataSummaryEntry2) {
            return Double.compare(doubleDataSummaryEntry.getKey(), doubleDataSummaryEntry2.getKey());
        }
    }
}
