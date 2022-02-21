'use strict';

const fs = require('fs');

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\n');
	const enhancement = lines[0];

	let grid = (lines.slice(2)).map(line => line.split(""));

	// For part 1, just change to 2 iterations.
	for (let i = 0; i < 50; i++) {
		grid = iterate(grid, enhancement, (i % 2 === 0 ? '.' : '#'));
	}
	console.log(calc(grid));
}

/**
 * Performs one iteration on the grid, given an enhancement key and what the outer layers of the grid are.
 */
function iterate(grid, enhancement, edge) {
	const opp_edge = (edge == '#' ? '.' : '#');
	const old_dim = grid.length;

	// Add edges
	grid.unshift(new Array(old_dim).fill(edge));
	grid.unshift(new Array(old_dim).fill(edge));
	grid.push(new Array(old_dim).fill(edge));
	grid.push(new Array(old_dim).fill(edge));

	for (const row of grid) {
		row.unshift(edge);
		row.unshift(edge);
		row.push(edge);
		row.push(edge);
	}

	const new_grid = [];

	for (let i = 1; i < old_dim + 3; i++) {
		const row = grid[i];
		const new_row = [opp_edge];
		for (let j = 1; j < row.length - 1; j++) {
			new_row.push(enhance(grid, i, j, enhancement, edge))
		}
		new_row.push(opp_edge);
		new_grid.push(new_row);
	}
	
	new_grid.unshift(new Array(old_dim + 4).fill(opp_edge));
	new_grid.push(new Array(old_dim + 4).fill(opp_edge));
	return new_grid;
}

/**
 * Given the grid and a location, finds what the enhancement should be.
 */
function enhance(grid, row, col, enhancement, edge) {
	let binary_index = 8;
	let sum = 0;
	for (let i = -1; i <= 1; i++) {
		for (let j = -1; j <= 1; j++) {
			const y = row + i, x = col + j;
			if (x < 0 || y < 0 || x >= grid.length || y >= grid.length) {
				sum += Math.pow(2, binary_index) * (edge == '#' ? 1 : 0);
			} else {
				sum += Math.pow(2, binary_index) * (grid[y][x] == '#' ? 1 : 0);
			}
			binary_index--;
		}
	}
	return enhancement[sum];
}

function display_grid(grid) {
	for (const line of grid) {
		console.log(line.join(""));
	}
	console.log("---------------------------------------");
}

/**
 * Finds the number of lit squares on a grid.
 */
function calc(grid) {
	let sum = 0;
	for (const line of grid) {
		for (const c of line) {
			if (c === '#') {
				sum++;
			}
		}
	}
	return sum;
}

main();