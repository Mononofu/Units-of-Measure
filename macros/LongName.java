package macroimpl;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
public @interface LongName {
    String value();
}
