import fs from 'fs';
import path from 'path';

const folder = './javascript/beerbusch';
const outputFolder = './javascript/beerbusch_formatiert';

async function main(){
    /// Loop all files in folder
    const files = fs.readdirSync(folder);
    
    for (const file of files) {
        // check if file is .csv
        if (!file.endsWith('.csv')) {
            continue;
        }
        console.log(file);
        // delete all Columns in .csv file where decond row is empty
        const filePath = path.join(folder, file);


        const data = fs.readFileSync(filePath, 'utf8');
        const lines = data.split(/\r\n|\n|\r/);
        console.log(lines.length);
        const header = lines[0].split(';');
        const secondRow = lines[2].split(';');
        const columnsToDelete = [];
        for (let i = 0; i < header.length; i++) {
            if (secondRow[i] === '') {
                columnsToDelete.push(i);
            }
        }
        console.log(columnsToDelete);
        // remove columns
        const newLines = [];
        for (const line of lines) {
            const columns = line.split(';');
            const newColumns = [];
            for (let i = 0; i < columns.length; i++) {
                if (!columnsToDelete.includes(i)) {
                    newColumns.push(columns[i]);
                }
            }
            newLines.push(newColumns.join(';'));
        }
        console.log(newLines);

        // write new file
        const newFilePath = path.join(outputFolder, file);
        fs.writeFileSync(newFilePath, newLines.join('\n'), 'utf8');
    }

    console.log('Done');
}

main();