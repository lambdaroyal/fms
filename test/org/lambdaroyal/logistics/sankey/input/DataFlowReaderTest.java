/**
 * Copyright 1999 Christian Meichsner. This file was created at 17.12.13
 * <p/>
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p/>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * <p/>
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.lambdaroyal.logistics.sankey.input;

import org.junit.Test;

import java.io.File;
import java.io.IOException;

/**
 * @author gix
 */
public class DataFlowReaderTest {
    @Test
    public void loadWorkbook() throws IOException, WorkbookLevelException {
        IDataFlow x = DataFlowReader.read(new File("input.xls"));

    }
}
