/**
 * Created with IntelliJ IDEA.
 * User: deathnik
 * Date: 9/25/13
 * Time: 4:22 PM
 * To change this template use File | Settings | File Templates.
 */
public class DataSummary {
    double data;
    int allEntries;
    int posEntries;

    public DataSummary(double data,int label) {
        this.data = data;
        this.allEntries = 0;
        this.posEntries = 0;
        this.addLabel(label);
    }

    public void addLabel(int label){
        this.incAllEntries();
        if(label == 1) this.incPosEntries();
    }

    public void incAllEntries() {
        allEntries++;
    }

    public void incPosEntries() {
        posEntries++;
    }

    public double getProbability() {
        return posEntries * 1.0 / allEntries;
    }
}
