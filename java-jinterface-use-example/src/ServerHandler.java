import com.ericsson.otp.erlang.*;

import java.io.IOException;

public class ServerHandler {

    private OtpMbox otpMbox;
    private OtpErlangPid otpErlangPid;
    private String registeredProcessName;
    private String eNodeName;

    public ServerHandler(String registeredProcessName, String eNodeName, String cookie) {
        System.setProperty("OtpConnection.trace", "0");
        try {
            OtpNode node = new OtpNode("jNode", cookie);
            if (node.ping(eNodeName, 10000)) {
                System.out.println("eNode is up");
                otpMbox = node.createMbox();
                otpErlangPid = otpMbox.self();
                otpMbox.registerName("javaProcess");
                this.registeredProcessName = registeredProcessName;
                this.eNodeName = eNodeName;
            }
            else {
                System.out.println("eNode is down");
            }
        } catch (IOException e) {
            System.out.println("Couldn't create node");
        }
    }

    public void stop() {
        call(getAtom("terminate"), null);
    }

    public void print() {
        call(getAtom("print"), null);
    }

    public void addStation(String stationName, int coordX, int coordY) {
        cast(getAtom("addStation"),
                new OtpErlangTuple(new OtpErlangObject[]{getString(stationName), getCoords(coordX, coordY)}));
    }

    public void addValue(String stationName, Integer[] date, String type, double value) {
        cast(getAtom("addValue"), new OtpErlangTuple(
                new OtpErlangObject[]{getString(stationName), getDate(date), getString(type), getDouble(value)}));
    }

    public void removeValue(String stationName, Integer[] date, String type) {
        cast(getAtom("removeValue"), new OtpErlangTuple(
                new OtpErlangObject[]{getString(stationName), getDate(date), getString(type)}));
    }

    public void getOneValue(String stationName, Integer[] date, String type) {
        call(getAtom("getOneValue"), new OtpErlangTuple(
                new OtpErlangObject[]{getString(stationName), getDate(date), getString(type)}));
    }

    public void getStationMean(String stationName, String type) {
        call(getAtom("getStationMean"), new OtpErlangTuple(
                new OtpErlangObject[]{getString(stationName), getString(type)}));
    }

    public void getDailyMean(String type, Integer[] day) {
        call(getAtom("getDailyMean"), new OtpErlangTuple(new OtpErlangObject[]{getString(type), getDay(day)}));
    }

    public void getHourlyMean(String type, int hour) {
        call(getAtom("getHourlyMean"), new OtpErlangTuple(new OtpErlangObject[]{getString(type), getInt(hour)}));
    }

    public void getDailyAverageDataCount(String stationName, Integer[] date) {
        call(getAtom("getDailyAverageDataCount"), new OtpErlangTuple(
                new OtpErlangObject[]{getString(stationName), getDate(date)}));
    }

    public void getDailyOverLimit(Integer[] day, String type, int limit) {
        call(getAtom("getDailyOverLimit"), new OtpErlangTuple(
                new OtpErlangObject[]{getDay(day), getString(type), getInt(limit)}));
    }

    public void getMaximumGradientStations(String type) {
        call(getAtom("getMaximumGradientStations"), new OtpErlangTuple(new OtpErlangObject[]{getString(type)}));
    }

    public void getMinValue(String type) {
        call(getAtom("getMinValue"), new OtpErlangTuple(new OtpErlangObject[]{getString(type)}));
    }

    public void getMaxValue(String type) {
        call(getAtom("getMaxValue"), new OtpErlangTuple(new OtpErlangObject[]{getString(type)}));
    }

    private void call(OtpErlangAtom fun, OtpErlangTuple args) {
        OtpErlangObject[] msg;
        if (args == null) {
            msg = new OtpErlangObject[]{otpErlangPid, fun};
        } else {
            msg = new OtpErlangObject[]{otpErlangPid, fun, args};
        }
        otpMbox.send(registeredProcessName, eNodeName, new OtpErlangTuple(msg));
        try {
            OtpErlangObject response = otpMbox.receive();
            if (response instanceof OtpErlangTuple) {
                System.out.println((((OtpErlangTuple) response).elementAt(1)));
            }
        }
        catch (OtpErlangExit e) {
            System.out.println("Remote pid " + e.pid() + " has terminated");
        }
        catch (OtpErlangDecodeException f) {
            System.out.println("Received message could not be decoded: " + f);
        }
    }

    private void cast(OtpErlangAtom fun, OtpErlangTuple args) {
        otpMbox.send(registeredProcessName, eNodeName,
                new OtpErlangTuple(new OtpErlangObject[]{otpErlangPid, fun, args}));
    }

    private OtpErlangString getString(String string) {
        return new OtpErlangString(string);
    }

    private OtpErlangInt getInt(int integer) {
        return new OtpErlangInt(integer);
    }

    private OtpErlangDouble getDouble(double num) {
        return new OtpErlangDouble(num);
    }

    private OtpErlangAtom getAtom(String atom) {
        return new OtpErlangAtom(atom);
    }

    private OtpErlangTuple getCoords(int coordX, int coordY) {
        OtpErlangObject[] stationCoords = { getInt(coordX), getInt(coordY) };
        return  new OtpErlangTuple(stationCoords);
    }

    private OtpErlangTuple getDay(Integer[] day) {
        return new OtpErlangTuple(new OtpErlangObject[]{getInt(day[0]), getInt(day[1]), getInt(day[2])});
    }

    private OtpErlangTuple getDate(Integer[] date) {
        OtpErlangObject[] DateList = {getInt(date[0]), getInt(date[1]), getInt(date[2])};
        OtpErlangObject[] TimeList = {getInt(date[3]), getInt(date[4]), getInt(date[5])};
        OtpErlangTuple dateTuple = new OtpErlangTuple(DateList);
        OtpErlangTuple timeTuple = new OtpErlangTuple(TimeList);
        OtpErlangObject[] allDateList = {dateTuple, timeTuple};
        return new OtpErlangTuple(allDateList);
    }
}
