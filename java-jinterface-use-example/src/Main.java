public class Main {

    public static void main(String[] args) {
        ServerHandler serverHandler = new ServerHandler(
                "server", "eNode", "erljava");

        Integer[] date = {12,4,5,23,34,23};
        Integer[] day = {12,4,5};
        int hour = 23;

        //serverHandler.addStation("StationName2", 3443, 3235);
        //serverHandler.addValue("StationName2", date, "TP24", 34.6);
        //serverHandler.removeValue("StationName2", date, "TP24");
        //serverHandler.getOneValue("StationName1", date, "TP25");
        //serverHandler.getStationMean("StationName1", "TP25");
        //serverHandler.getDailyMean("TP25", day);
        //serverHandler.getHourlyMean("TP25", hour);
        //serverHandler.getDailyAverageDataCount("StationName1", date);
        //serverHandler.getDailyOverLimit(date, "TP24", 10);
        //serverHandler.getMaximumGradientStations("TP24");
        //serverHandler.getMinValue("TP24");
        //serverHandler.getMaxValue("TP24");
        //serverHandler.print();
        //serverHandler.stop();
    }
}
