package com.github.keenon.minimalml;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by keenon on 12/3/14.
 *
 * Holds a plot, in the form of arrays etc. Outputs a folder with the required data and the script to graph it, as well
 * as the PNG output.
 */
public class GNUPlot {
    public String title = "Title";
    public String xLabel = "Time";
    public String yLabel = "Value";

    private List<double[]> xAxisData = new ArrayList<>();
    private List<double[]> yAxisData = new ArrayList<>();
    private List<Double> xLines = new ArrayList<>();
    private List<Double> yLines = new ArrayList<>();

    public void addLine(double[] xData, double[] yData) {
        assert(xData.length == yData.length);
        xAxisData.add(xData);
        yAxisData.add(yData);
    }

    public void addVerticalLine(double x) {
        xLines.add(x);
    }

    public void addHorizonalLine(double y) {
        yLines.add(y);
    }

    public void saveAnalysis(String folder) throws IOException {
        if (folder.endsWith("/")) folder = folder.substring(0, folder.length() - 1);
        folder = folder + "/" + title.replaceAll(" ", "_").replaceAll("/", "-");
        File f = new File(folder);
        if (!f.exists()) f.mkdirs();
        dumpDatFiles(folder);
        dumpScript(folder);
        Runtime.getRuntime().exec("gnuplot "+folder+"/gnuplot_script");
    }

    private void dumpScript(String folder) throws IOException {
        BufferedWriter bw = new BufferedWriter(new FileWriter(folder+"/gnuplot_script"));
        bw.append("set nokey\n");
        bw.append("set terminal png medium\n");
        bw.append("set output \"").append(folder).append("/").append(title.replaceAll(" ", "_").replaceAll("/", "-"))
                .append(".png\"\n");
        bw.append("set title '").append(title).append("'\n");
        bw.append("set xlabel '").append(xLabel).append("'\n");
        bw.append("set ylabel '").append(yLabel).append("'\n");

        double maxY = 0.0;
        double minY = Double.POSITIVE_INFINITY;
        for (double[] arr : yAxisData) for (double y : arr) if (y > maxY) maxY = y;
        for (double[] arr : yAxisData) for (double y : arr) if (y < minY) minY = y;
        for (double x : xLines) {
            bw.append("set arrow from "+x+","+minY+" to "+x+","+maxY+" nohead lc rgb 'purple'\n");
        }
        double maxX = 0.0;
        double minX = Double.POSITIVE_INFINITY;
        for (double[] arr : xAxisData) for (double x : arr) if (x > maxX) maxX = x;
        for (double[] arr : xAxisData) for (double x : arr) if (x < minX) minX = x;
        for (double y : yLines) {
            bw.append("set arrow from "+minX+","+y+" to "+maxX+","+y+" nohead lc rgb 'purple'\n");
        }
        bw.append("plot ");
        for (int i = 0; i < xAxisData.size(); i++) {
            bw.append("'").append(folder).append("/").append(Integer.toString(i)).append(".dat' with lines");
            if (i < xAxisData.size() - 1) bw.append(", \\\n");
        }
        bw.close();
    }

    private void dumpDatFiles(String folder) throws IOException {
        assert(xAxisData.size() == yAxisData.size());
        for (int i = 0; i < xAxisData.size(); i++) {
            BufferedWriter bw = new BufferedWriter(new FileWriter(folder+"/"+i+".dat"));
            double[] xData = xAxisData.get(i);
            double[] yData = yAxisData.get(i);
            assert(xData.length == yData.length);
            for (int j = 0; j < xData.length; j++) {
                bw.append(Double.toString(xData[j])).append("\t").append(Double.toString(yData[j])).append("\n");
            }
            bw.close();
        }
    }
}
