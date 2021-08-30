import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.IOException;

public class Hello {
    public static void main(String[] args) throws IOException {
        FileOutputStream out = new FileOutputStream(FileDescriptor.out);
        out.write("Hello, World!".getBytes());
        out.close();
    }
}
